#ifndef _SRC_UTILS_INCLUDE_REIFY_UTILS_THREAD_WITH_WORK_QUEUE_H_
#define _SRC_UTILS_INCLUDE_REIFY_UTILS_THREAD_WITH_WORK_QUEUE_H_

#include <condition_variable>
#include <functional>
#include <iostream>
#include <mutex>
#include <queue>
#include <thread>

#include "reify/utils/future.h"

namespace reify {
namespace utils {

class ThreadWithWorkQueue {
 public:
  ThreadWithWorkQueue();
  ~ThreadWithWorkQueue();

  void Push(const std::function<void()>& task);

  template <typename T>
  reify::utils::Future<T> Push(const std::function<T()>& task);

 private:
  std::queue<std::function<void()>> queue_;
  std::mutex mutex_;
  std::condition_variable queue_not_empty_;

  std::thread thread_;
};

template <typename T>
reify::utils::Future<T> ThreadWithWorkQueue::Push(
    const std::function<T()>& task) {
  if (!task) {
    return;
  }

  // Semantically this doesn't really need to be a shared_ptr. It is though
  // because std::function must be copyable, and this lets us achieve that.
  auto promise = std::make_shared<reify::utils::Promise<T>>();
  auto future = promise->future();

  Push([task, promise]() mutable { promise->set(task()); });

  return future;
}

}  // namespace utils
}  // namespace reify

#endif  // _SRC_UTILS_INCLUDE_REIFY_UTILS_THREAD_WITH_WORK_QUEUE_H_
