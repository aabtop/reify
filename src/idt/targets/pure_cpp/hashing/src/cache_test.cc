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
  reify::pure_cpp::Cache cache(&thread_pool);

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
  reify::pure_cpp::Cache cache(&thread_pool);

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

TEST(CacheTest, TestMemoryUsage) {
  ebb::ThreadPool thread_pool(1, DEFAULT_STACK_SIZE,
                              ebb::ThreadPool::SchedulingPolicy::LIFO);
  reify::pure_cpp::Cache cache(&thread_pool);

  cache.LookupOrCompute<int, int>(1, [] { return 1; });
  // See the definition above of EstimatedMemoryUsageInBytes(int).
  EXPECT_EQ(cache.MemoryUsageInBytes(), 100);

  cache.LookupOrCompute<int, int>(2, [] { return 2; });
  EXPECT_EQ(cache.MemoryUsageInBytes(), 300);

  // A cache hit should not result in a chance in cache memory usage.
  cache.LookupOrCompute<int, int>(1, [] { return 1; });
  EXPECT_EQ(cache.MemoryUsageInBytes(), 300);

  cache.LookupOrCompute<int, int>(5, [] { return 5; });
  EXPECT_EQ(cache.MemoryUsageInBytes(), 800);

  // LookupOnlys should certainly not result in a memory usage change.
  cache.LookupOnly<int, int>(5);
  EXPECT_EQ(cache.MemoryUsageInBytes(), 800);
  cache.LookupOnly<int, int>(6);
  EXPECT_EQ(cache.MemoryUsageInBytes(), 800);
}
}  // namespace