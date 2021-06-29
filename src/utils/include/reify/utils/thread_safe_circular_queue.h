#ifndef _SRC_UTILS_INCLUDE_REIFY_UTILS_THREAD_SAFE_CIRCULAR_QUEUE_H_
#define _SRC_UTILS_INCLUDE_REIFY_UTILS_THREAD_SAFE_CIRCULAR_QUEUE_H_

#include <condition_variable>
#include <mutex>

#include "reify/utils/circular_queue.h"

namespace reify {
namespace utils {

template <typename T, size_t N>
class ThreadSafeCircularQueue {
 public:
  void enqueue(const T& x) {
    std::unique_lock lock(mutex_);
    not_full_cond_.wait(lock, [this] { return !queue_.full(); });
    queue_.enqueue(x);
    not_empty_cond_.notify_all();
  }
  void enqueue(T&& x) {
    std::unique_lock lock(mutex_);
    not_full_cond_.wait(lock, [this] { return !queue_.full(); });
    queue_.enqueue(std::move(x));
    not_empty_cond_.notify_all();
  }
  T dequeue() {
    std::unique_lock lock(mutex_);
    not_empty_cond_.wait(lock, [this] { return !queue_.empty(); });
    T output = queue_.dequeue();
    not_full_cond_.notify_all();
    return std::move(output);
  }

 private:
  CircularQueue<T, N> queue_;
  std::mutex mutex_;
  std::condition_variable not_empty_cond_;
  std::condition_variable not_full_cond_;
};

template <size_t N>
class ThreadSafeCircularQueue<void, N> {
 public:
  void enqueue() { queue_.enqueue(true); }
  void dequeue() { queue_.dequeue(); }

 private:
  ThreadSafeCircularQueue<bool, N> queue_;
};

}  // namespace utils
}  // namespace reify

#endif  // _SRC_UTILS_INCLUDE_REIFY_UTILS_THREAD_SAFE_CIRCULAR_QUEUE_H_
