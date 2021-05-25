#include "reify/utils/future.h"

#include <gtest/gtest.h>

#include <thread>

// Demonstrate some basic assertions.
TEST(FutureTest, BasicTest) {
  reify::utils::Promise<int> promise;
  reify::utils::Future<int> future = promise.future();

  promise.set(5);
  EXPECT_EQ(5, std::get<1>(future.wait_and_get_results()));
}

TEST(FutureTest, SetBeforeFutureTest) {
  reify::utils::Promise<int> promise;
  promise.set(5);

  reify::utils::Future<int> future = promise.future();
  EXPECT_EQ(5, std::get<1>(future.wait_and_get_results()));
}

TEST(FutureTest, CancelTest) {
  reify::utils::Future<int> future = [] {
    reify::utils::Promise<int> promise;
    return promise.future();
  }();

  EXPECT_TRUE(std::holds_alternative<reify::utils::CancelledFuture>(
      future.wait_and_get_results()));
}

TEST(FutureTest, MultipleFuturesTest) {
  reify::utils::Promise<int> promise;
  reify::utils::Future<int> future1 = promise.future();
  reify::utils::Future<int> future2 = promise.future();

  promise.set(5);
  EXPECT_EQ(5, std::get<1>(future1.wait_and_get_results()));
  EXPECT_EQ(5, std::get<1>(future2.wait_and_get_results()));
}

TEST(FutureTest, CrossThreadTest) {
  reify::utils::Promise<int> promise;
  std::thread thread([&promise] { promise.set(5); });

  reify::utils::Future<int> future = promise.future();
  EXPECT_EQ(5, std::get<1>(future.wait_and_get_results()));

  thread.join();
}

TEST(FutureTest, WatchTest) {
  reify::utils::Promise<int> promise;
  reify::utils::Future<int> future = promise.future();

  std::optional<reify::utils::Future<int>::CancelledOrSharedResult>
      watch_result;
  reify::utils::Future<int>::Watch watch = future.watch(
      [&watch_result](const reify::utils::Future<int>::CancelledOrSharedResult&
                          maybe_result) { watch_result = maybe_result; });

  promise.set(5);

  reify::utils::Future<int>::CancelledOrResult wait_result =
      future.wait_and_get_results();

  ASSERT_TRUE(std::holds_alternative<int>(wait_result));
  EXPECT_EQ(5, std::get<1>(wait_result));
  ASSERT_TRUE(watch_result);
  ASSERT_TRUE(std::holds_alternative<std::shared_ptr<int>>(*watch_result));
  EXPECT_EQ(5, *std::get<1>(*watch_result));
}

TEST(FutureTest, WatchAfterSetTest) {
  reify::utils::Promise<int> promise;
  reify::utils::Future<int> future = promise.future();

  promise.set(5);

  std::optional<reify::utils::Future<int>::CancelledOrSharedResult>
      watch_result;
  reify::utils::Future<int>::Watch watch = future.watch(
      [&watch_result](const reify::utils::Future<int>::CancelledOrSharedResult&
                          maybe_result) { watch_result = maybe_result; });

  reify::utils::Future<int>::CancelledOrResult wait_result =
      future.wait_and_get_results();

  ASSERT_TRUE(std::holds_alternative<int>(wait_result));
  EXPECT_EQ(5, std::get<1>(wait_result));
  ASSERT_TRUE(watch_result);
  ASSERT_TRUE(std::holds_alternative<std::shared_ptr<int>>(*watch_result));
  EXPECT_EQ(5, *std::get<1>(*watch_result));
}

TEST(FutureTest, MovingWatchTest) {
  reify::utils::Promise<int> promise;
  reify::utils::Future<int> future = promise.future();

  std::optional<reify::utils::Future<int>::CancelledOrSharedResult>
      watch_result;
  // By assigning to an optional, we force a move constructor call.
  std::optional<reify::utils::Future<int>::Watch> watch = future.watch(
      [&watch_result](const reify::utils::Future<int>::CancelledOrSharedResult&
                          maybe_result) { watch_result = maybe_result; });

  promise.set(5);

  reify::utils::Future<int>::CancelledOrResult wait_result =
      future.wait_and_get_results();

  ASSERT_TRUE(std::holds_alternative<int>(wait_result));
  EXPECT_EQ(5, std::get<1>(wait_result));
  ASSERT_TRUE(watch_result);
  ASSERT_TRUE(std::holds_alternative<std::shared_ptr<int>>(*watch_result));
  EXPECT_EQ(5, *std::get<1>(*watch_result));
}
