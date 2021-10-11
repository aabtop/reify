#include "reify/pure_cpp/thread_pool_cache_runner.h"

#include <gtest/gtest.h>

#include "fiber_condition_variable.h"
#include "reify/pure_cpp/hashing_post_definitions.h"

namespace reify {
namespace pure_cpp {
template <>
int64_t EstimatedMemoryUsageInBytes(const int& x) {
  return x * 100;
}
}  // namespace pure_cpp

template <>
inline uint64_t HashObject(const int& input) {
  return static_cast<uint64_t>(input);
}

}  // namespace reify

namespace {

constexpr int DEFAULT_STACK_SIZE = 256 * 1024;

// Make sure that if many threads try to get an uncomputed value at the same
// time, it works and there's only one computation performed.
TEST(ThreadPoolCacheRunnerTest, TestSimultaneousCompute) {
  constexpr int THREAD_COUNT = 4;
  reify::pure_cpp::ThreadPoolCacheRunner runner(
      std::make_unique<ebb::ThreadPool>(
          THREAD_COUNT, DEFAULT_STACK_SIZE,
          ebb::ThreadPool::SchedulingPolicy::LIFO));

  int computations = 0;

  // Some tools to force the compute function processing to be delayed until
  // a time of our choosing.
  std::mutex cond_mutex;
  ebb::FiberConditionVariable main_waiting_cond(runner.thread_pool());

  std::unique_lock<std::mutex> main_lock(cond_mutex);

  auto compute_function = [&](reify::pure_cpp::ThreadPoolCacheRunner* runner,
                              const int& x) -> int {
    std::unique_lock<std::mutex> task_lock(cond_mutex);
    main_waiting_cond.notify_one();

    ++computations;

    return x;
  };

  reify::CachedHashReference<int> input_ref(5);
  std::vector<reify::pure_cpp::ThreadPoolCacheRunner::Future<
      std::shared_ptr<const int>>>
      futures;
  for (size_t i = 0; i < THREAD_COUNT; ++i) {
    futures.push_back(runner.MakeFuture<int, int>(compute_function, input_ref));
  }

  // Give our other tasks a chance to wait on the computation.
  std::this_thread::yield();
  std::this_thread::yield();

  // Okay, give the computation task a chance to execute now.
  main_waiting_cond.wait(main_lock);

  for (auto& future : futures) {
    EXPECT_EQ(*future.Get(), *input_ref);
  }
  EXPECT_EQ(computations, 1);
}

}  // namespace
