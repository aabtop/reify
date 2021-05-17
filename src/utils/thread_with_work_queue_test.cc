#include "reify/utils/thread_with_work_queue.h"

#include <gtest/gtest.h>

// Demonstrate some basic assertions.
TEST(ThreadWithWorkQueueTest, SimpleTest) {
  int value = 0;
  int intermediate_value = 0;

  {
    reify::utils::ThreadWithWorkQueue thread_with_work_queue;
    thread_with_work_queue.Enqueue([&] { value = 5; });
    thread_with_work_queue.Enqueue([&] { intermediate_value = value; });
    thread_with_work_queue.Enqueue([&] { value = 8; });
  }

  EXPECT_EQ(8, value);
  EXPECT_EQ(5, intermediate_value);
}

TEST(ThreadWithWorkQueueTest, FutureReturnValueTest) {
  int value = 0;

  reify::utils::ThreadWithWorkQueue thread_with_work_queue;
  thread_with_work_queue.Enqueue([&] { value = 5; });
  reify::utils::Future<int> a =
      thread_with_work_queue.Enqueue<int>([&] { return value; });
  reify::utils::Future<int> b =
      thread_with_work_queue.Enqueue<int>([&] { return ++value; });

  ASSERT_TRUE(std::holds_alternative<int>(a.wait_and_get_results()));
  EXPECT_EQ(5, std::get<int>(a.wait_and_get_results()));
  ASSERT_TRUE(std::holds_alternative<int>(b.wait_and_get_results()));
  EXPECT_EQ(6, std::get<int>(b.wait_and_get_results()));
  EXPECT_EQ(6, value);
}

TEST(ThreadWithWorkQueueTest, CanEnqueueFromWithinTest) {
  int value = 0;

  {
    reify::utils::ThreadWithWorkQueue thread_with_work_queue;
    thread_with_work_queue.Enqueue([&] {
      value = 5;
      thread_with_work_queue.Enqueue([&value]() { value += 3; });
    });
  }

  EXPECT_EQ(8, value);
}
