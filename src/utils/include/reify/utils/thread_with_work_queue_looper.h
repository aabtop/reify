#ifndef _SRC_UTILS_INCLUDE_REIFY_UTILS_THREAD_WITH_WORK_QUEUE_LOOPER_H_
#define _SRC_UTILS_INCLUDE_REIFY_UTILS_THREAD_WITH_WORK_QUEUE_LOOPER_H_

#include <atomic>
#include <functional>
#include <memory>
#include <mutex>
#include <optional>

#include "reify/utils/thread_safe_circular_queue.h"

namespace reify {
namespace utils {

// Repeatedly calls the specified function on the specified
// `ThreadWithWorkQueue` object. If the function optionally returns an error
// type, the `on_error` callback is called instead.
template <typename ErrorType>
class ThreadWithWorkQueueLooper {
 public:
  ThreadWithWorkQueueLooper(
      reify::utils::ThreadWithWorkQueue* thread,
      const std::function<std::optional<ErrorType>()> repeat_function,
      const std::function<void(const ErrorType&)>& on_error)
      : quit_flag_state_(new QuitFlagState()), thread_(thread) {
    loop_function_ = [repeat_function, on_error,
                      quit_flag_state = quit_flag_state_, this]() {
      std::lock_guard lock(quit_flag_state->mutex);
      if (quit_flag_state->quitting) {
        // If the quitting flag is set, return early to break the cycle.
        return;
      }

      auto maybe_error = repeat_function();
      if (maybe_error) {
        on_error(*maybe_error);
        // If an error occurred we return early and break the cycle here.
      } else {
        thread_->Enqueue(loop_function_);
      }
    };
    thread_->Enqueue(loop_function_);
  }
  ~ThreadWithWorkQueueLooper() {
    // Mark that we'd like to quit.
    quit_flag_state_->quitting = true;
    // Holding the lock for a moment guarantees that we are not in the loop
    // body at that moment, which means we're safe for if the loop is iterated
    // one more time (because it will early exit since `quitting` is true).
    std::lock_guard lock(quit_flag_state_->mutex);
  }

 private:
  struct QuitFlagState {
    std::mutex mutex;
    std::atomic<bool> quitting = false;
  };
  std::shared_ptr<QuitFlagState> quit_flag_state_;
  reify::utils::ThreadWithWorkQueue* thread_;
  std::function<void()> loop_function_;
};

}  // namespace utils
}  // namespace reify

#endif  // _SRC_UTILS_INCLUDE_REIFY_UTILS_THREAD_WITH_WORK_QUEUE_LOOPER_H_
