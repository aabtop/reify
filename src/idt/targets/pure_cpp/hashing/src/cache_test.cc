#include "reify/pure_cpp/cache.h"

#include <gtest/gtest.h>

namespace reify {
namespace pure_cpp {
template <>
int64_t EstimatedMemoryUsageInBytes(const int& x) {
  return x * 100;
}
}  // namespace pure_cpp
}  // namespace reify

namespace {

constexpr int DEFAULT_STACK_SIZE = 256 * 1024;

TEST(CacheTest, TestSimpleCache) {
  ebb::ThreadPool thread_pool(1, DEFAULT_STACK_SIZE,
                              ebb::ThreadPool::SchedulingPolicy::LIFO);
  reify::pure_cpp::Cache cache(&thread_pool, 1000000);

  int computations = 0;
  auto compute_function = [&computations]() {
    ++computations;
    return 10;
  };

  auto result1 = cache.LookupOrCompute<int, int>(1, compute_function);
  EXPECT_EQ(*result1, 10);
  EXPECT_EQ(computations, 1);
  auto result2 = cache.LookupOrCompute<int, int>(1, compute_function);
  EXPECT_EQ(*result2, 10);
  EXPECT_EQ(computations, 1);

  auto result3 = cache.LookupOrCompute<int, int>(2, compute_function);
  EXPECT_EQ(*result3, 10);
  EXPECT_EQ(computations, 2);
}

TEST(CacheTest, TestSimpleLookupOnly) {
  ebb::ThreadPool thread_pool(1, DEFAULT_STACK_SIZE,
                              ebb::ThreadPool::SchedulingPolicy::LIFO);
  reify::pure_cpp::Cache cache(&thread_pool, 1000000);

  int computations = 0;
  auto compute_function = [&computations] {
    ++computations;
    return 10;
  };

  auto result1 = cache.LookupOnly<int, int>(1);
  EXPECT_EQ(result1, nullptr);

  auto result2 = cache.LookupOrCompute<int, int>(1, compute_function);
  EXPECT_EQ(*result2, 10);
  EXPECT_EQ(computations, 1);

  auto result3 = cache.LookupOnly<int, int>(1);
  ASSERT_NE(result3, nullptr);
  EXPECT_EQ(*result3, 10);
  EXPECT_EQ(computations, 1);
}

TEST(CacheTest, TestEstimatedMemoryUsage) {
  ebb::ThreadPool thread_pool(1, DEFAULT_STACK_SIZE,
                              ebb::ThreadPool::SchedulingPolicy::LIFO);
  reify::pure_cpp::Cache cache(&thread_pool, 1000000);

  cache.LookupOrCompute<int, int>(1, [] { return 1; });
  // See the definition above of EstimatedMemoryUsageInBytes(int).
  EXPECT_EQ(cache.EstimatedMemoryUsageInBytes(), 100);

  cache.LookupOrCompute<int, int>(2, [] { return 2; });
  EXPECT_EQ(cache.EstimatedMemoryUsageInBytes(), 300);

  // A cache hit should not result in a chance in cache memory usage.
  cache.LookupOrCompute<int, int>(1, [] { return 1; });
  EXPECT_EQ(cache.EstimatedMemoryUsageInBytes(), 300);

  cache.LookupOrCompute<int, int>(5, [] { return 5; });
  EXPECT_EQ(cache.EstimatedMemoryUsageInBytes(), 800);

  // LookupOnlys should certainly not result in a memory usage change.
  cache.LookupOnly<int, int>(5);
  EXPECT_EQ(cache.EstimatedMemoryUsageInBytes(), 800);
  cache.LookupOnly<int, int>(6);
  EXPECT_EQ(cache.EstimatedMemoryUsageInBytes(), 800);
}

TEST(CacheTest, TestPurging) {
  ebb::ThreadPool thread_pool(1, DEFAULT_STACK_SIZE,
                              ebb::ThreadPool::SchedulingPolicy::LIFO);
  reify::pure_cpp::Cache cache(&thread_pool, 500);

  int computations = 0;
  auto increment_computations_and_return = [&computations](int x) {
    return [&computations, &x]() {
      ++computations;
      return x;
    };
  };

  cache.LookupOrCompute<int, int>(1, increment_computations_and_return(1));
  EXPECT_EQ(cache.EstimatedMemoryUsageInBytes(), 100);
  EXPECT_EQ(computations, 1);

  // We expect the first element to now have been evicted.
  cache.LookupOrCompute<int, int>(5, increment_computations_and_return(5));
  EXPECT_EQ(cache.EstimatedMemoryUsageInBytes(), 500);
  EXPECT_EQ(computations, 2);

  // There's no room for the first and the second element, so we evict the
  // second and recompute the first.
  cache.LookupOrCompute<int, int>(1, increment_computations_and_return(1));
  EXPECT_EQ(cache.EstimatedMemoryUsageInBytes(), 100);
  EXPECT_EQ(computations, 3);

  // No need to recompute the same element, nothing changes here.
  cache.LookupOrCompute<int, int>(1, increment_computations_and_return(1));
  EXPECT_EQ(cache.EstimatedMemoryUsageInBytes(), 100);
  EXPECT_EQ(computations, 3);

  // We can fit multiple elements in the cache at the same time, as long as
  // we don't go over the capacity.
  cache.LookupOrCompute<int, int>(3, increment_computations_and_return(3));
  EXPECT_EQ(cache.EstimatedMemoryUsageInBytes(), 400);
  EXPECT_EQ(computations, 4);

  // And if we go over be a bit, we only purge until we can fit again.
  cache.LookupOrCompute<int, int>(2, increment_computations_and_return(2));
  EXPECT_EQ(cache.EstimatedMemoryUsageInBytes(), 500);
  EXPECT_EQ(computations, 5);

  // We can pull old elements out of the cache that haven't been purged yet.
  cache.LookupOrCompute<int, int>(3, increment_computations_and_return(3));
  EXPECT_EQ(cache.EstimatedMemoryUsageInBytes(), 500);
  EXPECT_EQ(computations, 5);

  // We can purge multiple elements at once, if we need to.
  cache.LookupOrCompute<int, int>(5, increment_computations_and_return(5));
  EXPECT_EQ(cache.EstimatedMemoryUsageInBytes(), 500);
  EXPECT_EQ(computations, 6);
}

}  // namespace
