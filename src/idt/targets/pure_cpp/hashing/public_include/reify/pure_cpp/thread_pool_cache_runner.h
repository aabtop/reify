#ifndef _REIFY_IDT_TARGETS_PURE_CPP_THREAD_POOL_CACHE_RUNNER_H_
#define _REIFY_IDT_TARGETS_PURE_CPP_THREAD_POOL_CACHE_RUNNER_H_

#include "fiber_condition_variable.h"
#include "reify/pure_cpp/cache.h"
#include "reify/pure_cpp/hashing.h"
#include "thread_pool.h"

namespace reify {
namespace pure_cpp {

class ThreadPoolCacheRunner {
 public:
  ThreadPoolCacheRunner(std::unique_ptr<ebb::ThreadPool> thread_pool)
      : thread_pool_(std::move(thread_pool)), cache_(thread_pool_.get()) {}

  template <typename R>
  class Future {
   public:
    Future(Future&&) = default;
    ~Future() {
      if (auto task_data_ptr = std::get_if<1>(&result_)) {
        const auto& task_data = *task_data_ptr;
        // Check if we got moved or not.
        if (task_data) {
          // Wait for the future to complete, but do nothing with the results.
          std::unique_lock<std::mutex> lock(task_data->mutex);
          while (!task_data->result) {
            task_data->cond.wait(lock);
          }
        }
      }
    }

    // Waits for the processing to finish and returns the result.
    const R& Get() {
      if (auto result = std::get_if<0>(&result_)) {
        // Trivially constructed, we can return immediately.
        return *result;
      } else {
        const auto& task_data = std::get<1>(result_);
        std::unique_lock<std::mutex> lock(task_data->mutex);
        while (!task_data->result) {
          task_data->cond.wait(lock);
        }
        if (auto exception =
                std::get_if<std::exception_ptr>(&(*task_data->result))) {
          // Rethrow the exception in the parent context.
          std::rethrow_exception(*exception);
        } else {
          return std::get<R>(*task_data->result);
        }
      }
    }

   private:
    friend class ThreadPoolCacheRunner;

    struct TaskData {
      TaskData(ThreadPoolCacheRunner* runner, const std::function<R()>& f)
          : cond(runner->thread_pool_.get()),
            task(runner->thread_pool_.get(), [this, f]() {
              try {
                R computed_result = f();
                std::lock_guard<std::mutex> lock(mutex);
                result = std::move(computed_result);
                cond.notify_one();
              } catch (...) {
                // Save the exception so that we can forward it on to anyone
                // trying to get the data out of the Future.
                std::exception_ptr ep = std::current_exception();
                std::lock_guard<std::mutex> lock(mutex);
                result = std::move(ep);
                cond.notify_one();
              }
            }) {}

      std::mutex mutex;
      ebb::FiberConditionVariable cond;
      std::optional<std::variant<R, std::exception_ptr>> result;
      ebb::ThreadPool::Task task;
    };

    Future(const R& result) : result_(result) {}
    Future(ThreadPoolCacheRunner* runner, const std::function<R()>& f)
        : result_(std::make_unique<TaskData>(runner, f)) {}

    // We use a unique_ptr here instead of a optional so that Futures can be
    // movable.
    using ImmediateResultOrTask = std::variant<R, std::unique_ptr<TaskData>>;
    ImmediateResultOrTask result_;
  };

  template <typename R, typename T>
  Future<std::shared_ptr<const R>> MakeFuture(
      R (*compute)(ThreadPoolCacheRunner*, const T&),
      const reify::CachedHashReference<T>& x) {
    // Before spawning a new task, quickly see if we have it cached already
    // and just return that.
    if (auto cache_hit = cache_.LookupOnly<R, T>(x.hash())) {
      return Future<std::shared_ptr<const R>>(cache_hit);
    }
    // Okay no quick cache lookup, let's spawn a fiber to work on this one.
    return Future<std::shared_ptr<const R>>(this, [this, compute, x]() {
      return cache_.LookupOrCompute<R, T>(x.hash(),
                                          [&]() { return compute(this, *x); });
    });
  }

  // We don't deal with caching if the input is not a cached reference.
  template <typename R, typename T>
  Future<std::shared_ptr<const R>> MakeFuture(
      R (*compute)(ThreadPoolCacheRunner*, const T&), const T& x) {
    return Future<std::shared_ptr<const R>>(this, [this, compute, x]() {
      return std::make_shared<const R>(compute(this, x));
    });
  }

  template <typename R, typename T>
  Future<std::shared_ptr<const R>> MakeFutureWithoutCaching(
      R (*compute)(ThreadPoolCacheRunner*, const T&), const T& x) {
    return Future<std::shared_ptr<const R>>(this, [this, compute, x]() {
      return std::make_shared<const R>(compute(this, x));
    });
  }
  template <typename R, typename T>
  Future<std::shared_ptr<const R>> MakeFutureWithoutCaching(
      R (*compute)(ThreadPoolCacheRunner*, const T&),
      const reify::CachedHashReference<T>& x) {
    return Future<std::shared_ptr<const R>>(this, [this, compute, x]() {
      return std::make_shared<const R>(compute(this, *x));
    });
  }

 private:
  std::unique_ptr<ebb::ThreadPool> thread_pool_;
  Cache cache_;
};

}  // namespace pure_cpp
}  // namespace reify

#endif  // _REIFY_IDT_TARGETS_PURE_CPP_THREAD_POOL_CACHE_RUNNER_H_
