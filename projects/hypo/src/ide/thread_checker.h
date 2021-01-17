#ifndef _IDE_THREAD_CHECKER_H_
#define _IDE_THREAD_CHECKER_H_

#include <optional>
#include <thread>

class ThreadChecker {
 public:
  void Check() {
    if (!thread_id_) {
      thread_id_ = std::this_thread::get_id();
    } else {
      assert(thread_id_ == std::this_thread::get_id());
    }
  }

 private:
  std::optional<std::thread::id> thread_id_;
};

#endif  // _IDE_THREAD_CHECKER_H_
