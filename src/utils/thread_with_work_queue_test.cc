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
      thread_with_work_queue.EnqueueWithResult<int>([&] { return value; });
  reify::utils::Future<int> b =
      thread_with_work_queue.EnqueueWithResult<int>([&] { return ++value; });

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

TEST(ThreadWithWorkQueueTest, ScopedWorkQueueCleansUpItself) {
  int value = -1;

  std::mutex mutex;
  std::condition_variable latched_cond;
  std::condition_variable unlatched_cond;
  bool latched = false;

  auto latch_and_wait = [&] {
    std::unique_lock<std::mutex> lock(mutex);
    ASSERT_FALSE(latched);
    latched = true;
    latched_cond.notify_all();
    unlatched_cond.wait(lock, [&latched] { return !latched; });
  };
  auto wait_for_latched = [&] {
    std::unique_lock<std::mutex> lock(mutex);
    latched_cond.wait(lock, [&latched] { return latched; });
  };
  auto unlatch = [&] {
    std::unique_lock<std::mutex> lock(mutex);
    ASSERT_TRUE(latched);
    latched = false;
    unlatched_cond.notify_all();
  };

  // We do most of this test in a separate thread so that it can block.
  reify::utils::ThreadWithWorkQueue outer_thread;
  outer_thread.Enqueue([&] {
    reify::utils::ThreadWithWorkQueue thread_with_work_queue;
    thread_with_work_queue.Enqueue([&] {
      latch_and_wait();
      value = 1;
    });

    {
      int foo = 2;
      reify::utils::ScopedWorkQueue scoped_queue(&thread_with_work_queue);
      scoped_queue.Enqueue([&] {
        latch_and_wait();
        value = foo;
      });
      scoped_queue.Enqueue([&] {
        latch_and_wait();
        foo = 3;
      });
      scoped_queue.Enqueue([&] {
        latch_and_wait();
        value = foo;
      });
    }
    EXPECT_EQ(3, value);
    thread_with_work_queue.Enqueue([&] {
      latch_and_wait();
      value = 4;
      latch_and_wait();
    });
  });

  wait_for_latched();
  unlatch();
  wait_for_latched();
  EXPECT_EQ(1, value);
  unlatch();
  wait_for_latched();
  EXPECT_EQ(2, value);
  unlatch();
  wait_for_latched();
  EXPECT_EQ(2, value);
  unlatch();
  wait_for_latched();
  EXPECT_EQ(3, value);
  unlatch();
  wait_for_latched();
  EXPECT_EQ(4, value);
  unlatch();
}
