#ifndef _REIFY_IDT_TARGETS_PURE_CPP_CACHE_H_
#define _REIFY_IDT_TARGETS_PURE_CPP_CACHE_H_

#include <any>

#include "fiber_condition_variable.h"
#include "reify/pure_cpp/hashing.h"

namespace reify {
namespace pure_cpp {

class Cache {
 public:
  Cache(ebb::ThreadPool* thread_pool) : thread_pool_(thread_pool) {}

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
      auto result = std::make_shared<const R>(compute());
      monitor_mutex_.lock();

      // Replace the wait list in the cache table with the final results.
      inserted->second = ResultOrWaitList(std::in_place_index<0>, result);

      // And for all our friends who were waiting on the results, make it
      // directly available to them.
      wait_list->result = result;

      // And wake up our friends.
      for (int i = 0; i < wait_list->num_waiters; ++i) {
        wait_list->cond.notify_one();
      }

      // And return the result.
      return result;
    } else {
      if (auto result = std::get_if<0>(&cache_hit->second)) {
        // We got a cache hit, return it, we're done!
        return std::any_cast<std::shared_ptr<const R>>(*result);
      } else {
        // Looks like another fiber is computing the results, so we must wait
        // for it.
        auto wait_list = std::get<1>(cache_hit->second);
        ++wait_list->num_waiters;
        wait_list->cond.wait(lock);

        // Since we were waiting in line, we get a special direct delivery
        // of the results waiting for us, so just return that.
        return std::any_cast<std::shared_ptr<const R>>(wait_list->result);
      }
    }
    return std::any_cast<std::shared_ptr<const R>>(cache_hit->second);
  }

  template <typename R, typename T>
  std::shared_ptr<const R> LookupOnly(uint64_t hash) {
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
      return std::any_cast<std::shared_ptr<const R>>(*cache_hit);
    } else {
      return nullptr;
    }
  }

 private:
  template <typename T>
  static intptr_t TypeId() {
    static int foo;
    return reinterpret_cast<intptr_t>(&foo);
  }

  // Needed to instantiate condition variables.
  ebb::ThreadPool* thread_pool_;
  // Wraps all public method calls.
  std::mutex monitor_mutex_;

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
  using CacheMap = std::unordered_map<intptr_t, CachePerType>;

  CacheMap cache_;
};

}  // namespace pure_cpp
}  // namespace reify

#endif  // _REIFY_IDT_TARGETS_PURE_CPP_CACHE_H_
