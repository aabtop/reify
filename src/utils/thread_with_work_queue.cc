#include "reify/utils/thread_with_work_queue.h"

namespace reify {
namespace utils {

ThreadWithWorkQueue::ThreadWithWorkQueue()
    : thread_([this] {
        // Setup the thread run loop.

        while (true) {
          auto next_task = [this]() {
            std::unique_lock lock(mutex_);
            queue_not_empty_.wait(lock, [this] { return !queue_.empty(); });
            auto next_task = queue_.front();
            queue_.pop();
            if (queue_.empty()) {
              queue_empty_.notify_all();
            }
            return next_task;
          }();

          if (!next_task) {
            // This is the exit signal.
            return;
          }

          // Execute the task.
          next_task();
        }
      }) {}

ThreadWithWorkQueue::~ThreadWithWorkQueue() {
  {
    std::unique_lock lock(mutex_);
    queue_empty_.wait(lock, [this] { return queue_.empty(); });
    queue_.push(std::function<void()>());
    queue_not_empty_.notify_all();
  }
  thread_.join();
}

void ThreadWithWorkQueue::Enqueue(const std::function<void()>& task) {
  assert(task);

  std::lock_guard lock(mutex_);
  queue_.push(task);
  queue_not_empty_.notify_all();
}

}  // namespace utils
}  // namespace reify
