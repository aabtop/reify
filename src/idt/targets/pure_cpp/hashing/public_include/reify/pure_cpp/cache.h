#ifndef _REIFY_IDT_TARGETS_PURE_CPP_CACHE_H_
#define _REIFY_IDT_TARGETS_PURE_CPP_CACHE_H_

#include <any>
#include <list>

#include "fiber_condition_variable.h"
#include "reify/pure_cpp/hashing.h"

namespace reify {
namespace pure_cpp {

template <typename T>
int64_t EstimatedMemoryUsageInBytes(const T& x) {
  struct Foo {};
  static_assert(std::is_same<T, Foo>::value,
                "You must define a customization of this function, "
                "EstimatedMemoryUsageInBytes() for your type T.");
}

class Cache {
 public:
  Cache(ebb::ThreadPool* thread_pool, int64_t capacity)
      : thread_pool_(thread_pool), capacity_(capacity) {}

  template <typename R, typename T>
  std::shared_ptr<const R> LookupOrCompute(uint64_t hash,
                                           const std::function<R()>& compute) {
    std::unique_lock<std::mutex> lock(monitor_mutex_);
    auto& cache_per_type = cache_[TypeId<T>()];
    auto cache_hit = cache_per_type.find(hash);
    if (cache_hit == cache_per_type.end()) {
      // Looks like we're the lucky one who gets to compute it!
      // First create our wait list for others while we are computing.
      auto wait_list = std::make_shared<WaitList>(thread_pool_);
      auto [inserted, _] = cache_per_type.insert({hash, wait_list});

      monitor_mutex_.unlock();
      std::shared_ptr<const R> maybe_result;
      std::any result_or_exception;
      try {
        maybe_result = std::make_shared<const R>(compute());
        result_or_exception =
            maybe_result;  // At this point it's actually a "for_sure_result".
      } catch (...) {
        result_or_exception = std::current_exception();
      }
      monitor_mutex_.lock();

      int64_t estimated_memory_usage_in_bytes = [&maybe_result]() -> int64_t {
        if (maybe_result) {
          // If we successfully computed a new object to cache, update the
          // number of cached bytes we're currently storing, and maybe purge
          // cache items if we've gone over our capacity.
          return reify::pure_cpp::EstimatedMemoryUsageInBytes(*maybe_result);
        } else {
          // If this is just an exception, return a very small (though somewhat
          // arbitrary) amount of memory usage.
          return 16;
        }
      }();
      // Don't cache something that requires more memory than the cache's
      // capacity.
      bool should_cache = estimated_memory_usage_in_bytes <= capacity_;

      if (should_cache) {
        // Replace the wait list in the cache table with the final results.
        inserted->second =
            ResultOrWaitList(std::in_place_index<0>, result_or_exception);
        ordering_.push_front(
            {&cache_per_type, hash, estimated_memory_usage_in_bytes});
        approximate_cache_usage_ += estimated_memory_usage_in_bytes;
      } else {
        // Get rid of the cached waitlist, this entry is too big to fit in the
        // cache.
        cache_per_type.erase(inserted);
      }

      // And for all our friends who were waiting on the results, make it
      // directly available to them.
      wait_list->result = result_or_exception;

      // And wake up our friends.
      for (int i = 0; i < wait_list->num_waiters; ++i) {
        wait_list->cond.notify_one();
      }

      if (maybe_result) {
        // We may have added an entry to the cache, so check if we need to purge
        // it down or not now.
        PurgeToCapacity(std::move(lock));
        // And finally return the result or throw the exception.
        return maybe_result;
      } else {
        std::rethrow_exception(
            std::any_cast<std::exception_ptr>(result_or_exception));
      }
    } else {
      if (auto result = std::get_if<0>(&cache_hit->second)) {
        if (auto e = std::any_cast<std::exception_ptr>(&(*result))) {
          std::rethrow_exception(*e);
        }
        // We got a cache hit, return it, we're done!
        return std::any_cast<std::shared_ptr<const R>>(*result);
      } else {
        // Looks like another fiber is computing the results, so we must wait
        // for it.
        auto wait_list = std::get<1>(cache_hit->second);
        ++wait_list->num_waiters;
        wait_list->cond.wait(lock);

        if (auto e = std::any_cast<std::exception_ptr>(&wait_list->result)) {
          std::rethrow_exception(*e);
        }

        // Since we were waiting in line, we get a special direct delivery
        // of the results waiting for us, so just return that.
        return std::any_cast<std::shared_ptr<const R>>(wait_list->result);
      }
    }
  }

  template <typename R, typename T>
  std::shared_ptr<const R> LookupOnly(uint64_t hash) const {
    std::lock_guard<std::mutex> lock(monitor_mutex_);
    auto cache_per_type = cache_.find(TypeId<T>());
    if (cache_per_type == cache_.end()) {
      return nullptr;
    }
    auto cache_hit_or_wait_list = cache_per_type->second.find(hash);
    if (cache_hit_or_wait_list == cache_per_type->second.end()) {
      return nullptr;
    }
    if (auto cache_hit = std::get_if<0>(&cache_hit_or_wait_list->second)) {
      if (auto e = std::any_cast<std::exception_ptr>(&(*cache_hit))) {
        std::rethrow_exception(*e);
      }

      return std::any_cast<std::shared_ptr<const R>>(*cache_hit);
    } else {
      return nullptr;
    }
  }

  int64_t EstimatedMemoryUsageInBytes() const {
    std::lock_guard<std::mutex> lock(monitor_mutex_);
    return approximate_cache_usage_;
  }

  int64_t Capacity() const {
    std::lock_guard<std::mutex> lock(monitor_mutex_);
    return capacity_;
  }

  void SetCapacity(int64_t capacity) {
    assert(capacity >= 0);
    std::unique_lock<std::mutex> lock(monitor_mutex_);
    capacity_ = capacity;
    // In case capacity has been reduced here, start purging.
    PurgeToCapacity(std::move(lock));
  }

 private:
  using CacheItemTypeId = intptr_t;

  struct WaitList {
    WaitList(ebb::ThreadPool* thread_pool) : cond(thread_pool) {}

    // The idea is that everyone will wait on this condition variable.
    ebb::FiberConditionVariable cond;
    // ebb::FiberConditionVariable doesn't (currently) have a `notify_all()`
    // function :(.
    int num_waiters = 0;
    // We package the result here so that we can guarantee that it becomes
    // available to everyone who was waiting on it.
    std::any result;
  };
  using ResultOrWaitList = std::variant<std::any, std::shared_ptr<WaitList>>;
  using CachePerType = std::unordered_map<uint64_t, ResultOrWaitList>;
  using CacheMap = std::unordered_map<CacheItemTypeId, CachePerType>;

  struct OrderingInfo {
    // We need to maintain references back to the hash map it's stored in, so
    // we can find it in case we need to purge it.
    CachePerType* cache;
    uint64_t hash;
    int64_t estimated_memory_usage_in_bytes;
  };

  // This container keeps track of the order that elements should be purged...
  // It is maintained as a least recently used (LRU) container.
  using OrderingContainer = std::list<OrderingInfo>;

  template <typename T>
  static CacheItemTypeId TypeId() {
    static int foo;
    return reinterpret_cast<CacheItemTypeId>(&foo);
  }

  // Remove elements from the cache until we have a total approximate cache
  // usage of less than or equal to the capacity. The lock moved into this
  // function will be released before the function returns, so that we can
  // call the destructors of the released elements without holding the lock.
  void PurgeToCapacity(std::unique_lock<std::mutex> lock_to_release);

  // Needed to instantiate condition variables.
  ebb::ThreadPool* thread_pool_;
  // Wraps all public method calls.
  mutable std::mutex monitor_mutex_;

  CacheMap cache_;

  // We will make sure that the cache (but not everything else, no magic here)
  // never exceeds this set capacity. This essentially dictates when and how
  // much we purge elements from the cache.
  int64_t capacity_;

  // This datastructure keeps track of the LRU order of the cache elements.
  // Each element in it keeps a reference to the elements in the CacheMap
  // above so that it can remove the least recently used elements from it
  // during a purge if it needs to.
  OrderingContainer ordering_;

  int64_t approximate_cache_usage_ = 0;
};

}  // namespace pure_cpp
}  // namespace reify

#endif  // _REIFY_IDT_TARGETS_PURE_CPP_CACHE_H_
