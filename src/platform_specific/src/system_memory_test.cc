#include "platform_specific/system_memory.h"

#include <gtest/gtest.h>

#include <iostream>

namespace {
TEST(SystemMemoryTest, TestTotalSystemMemoryCapacityInBytes) {
  // Not much more we can do other than assert it's non-zero.
  std::cerr << "memory size: "
            << reify::platform_specific::TotalSystemMemoryCapacityInBytes()
            << std::endl;
  EXPECT_GT(reify::platform_specific::TotalSystemMemoryCapacityInBytes(), 0);
}

}  // namespace
