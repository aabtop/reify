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

class WorkQueue {
 public:
  virtual void Enqueue(const std::function<void()>& task) = 0;

  template <typename T>
  Future<T> EnqueueWithResult(const std::function<T()>& task);
};

template <typename T>
Future<T> WorkQueue::EnqueueWithResult(const std::function<T()>& task) {
  assert(task);

  // Semantically this doesn't really need to be a shared_ptr. It is though
  // because std::function must be copyable, and this lets us achieve that.
  auto promise = std::make_shared<Promise<T>>();
  auto future = promise->future();

  Enqueue([task, promise]() mutable { promise->set(task()); });

  return future;
}

class ThreadWithWorkQueue : public WorkQueue {
 public:
  ThreadWithWorkQueue();
  ~ThreadWithWorkQueue();

  void Enqueue(const std::function<void()>& task) override;

 private:
  std::queue<std::function<void()>> queue_;
  std::mutex mutex_;
  std::condition_variable queue_not_empty_;
  std::condition_variable queue_empty_;

  std::thread thread_;
};

// Wraps an existing work queue and forwards enqueings onto it.  When this
// scoped work queue is destroyed, it ensures that all messages enqueued through
// it are completed first, though it doesn't wait for the wrapped queue to be
// empty.
class ScopedWorkQueue : public WorkQueue {
 public:
  ScopedWorkQueue(WorkQueue* wrapped);
  ScopedWorkQueue(std::function<void(std::function<void()>)> enqueue_function);

  ~ScopedWorkQueue();

  void Enqueue(const std::function<void()>& task) override;

 private:
  std::mutex mutex_;
  std::condition_variable empty_cond_;
  int pending_message_count_ = 0;

  std::function<void(std::function<void()>)> enqueue_function_;
};

}  // namespace utils
}  // namespace reify

#endif  // _SRC_UTILS_INCLUDE_REIFY_UTILS_THREAD_WITH_WORK_QUEUE_H_
