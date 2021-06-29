#ifndef _SRC_UTILS_INCLUDE_REIFY_UTILS_FUTURE_H_
#define _SRC_UTILS_INCLUDE_REIFY_UTILS_FUTURE_H_

#include <cassert>
#include <condition_variable>
#include <functional>
#include <memory>
#include <mutex>
#include <optional>
#include <unordered_set>
#include <variant>

namespace reify {
namespace utils {

struct CancelledFuture {};

template <typename T>
class Future {
 public:
  class Watch;
  using CancelledOrResult = std::variant<CancelledFuture, T>;
  using CancelledOrSharedResult =
      std::variant<CancelledFuture, std::shared_ptr<T>>;

 private:
  struct SharedState {
    static CancelledOrSharedResult GetCancelledOrSharedResult(
        const std::shared_ptr<SharedState>& shared_state);

    std::mutex mutex;
    std::condition_variable ready_condition;
    std::unordered_set<std::function<void(const CancelledOrSharedResult&)>*>
        watches;

    std::optional<CancelledOrResult> result;
  };

 public:
  class Watch {
   public:
    Watch(const Watch&) = delete;
    Watch& operator=(const Watch&) = delete;
    Watch(Watch&&) = default;
    Watch& operator=(Watch&&) = default;
    ~Watch();

   private:
    template <typename U>
    friend class Promise;
    friend class Future<T>;

    Watch(const std::shared_ptr<SharedState>& shared_state,
          const std::function<void(const CancelledOrSharedResult&)>& on_ready);

    std::shared_ptr<SharedState> shared_state_;
    std::unique_ptr<std::function<void(const CancelledOrSharedResult&)>>
        on_ready_;
  };

  Watch watch(
      const std::function<void(const CancelledOrSharedResult&)>& on_ready);

  const CancelledOrResult& wait_and_get_results();

 private:
  template <typename U>
  friend class Promise;

  Future(const std::shared_ptr<SharedState>& shared_state)
      : shared_state_(shared_state) {}

  std::shared_ptr<SharedState> shared_state_;
};

template <typename T>
class Promise {
 public:
  Promise();
  Promise(const Promise&) = delete;
  Promise& operator=(const Promise&) = delete;
  Promise(Promise&&) = delete;
  Promise& operator=(Promise&&) = delete;
  ~Promise();

  void set(T&& x);

  Future<T> future();

 private:
  using SharedState = typename Future<T>::SharedState;

  std::shared_ptr<SharedState> shared_state_;
};

template <typename T>
const typename Future<T>::CancelledOrResult& Future<T>::wait_and_get_results() {
  std::unique_lock<std::mutex> lock(shared_state_->mutex);
  shared_state_->ready_condition.wait(
      lock, [this] { return !!shared_state_->result; });
  return *shared_state_->result;
}

template <typename T>
typename Future<T>::CancelledOrSharedResult
Future<T>::SharedState::GetCancelledOrSharedResult(
    const std::shared_ptr<SharedState>& shared_state) {
  assert(shared_state->result);
  auto& result = *shared_state->result;
  if (auto cancelled = std::get_if<0>(&result)) {
    return *cancelled;
  } else {
    return std::shared_ptr<T>(shared_state,
                              &(std::get<1>(*shared_state->result)));
  }
}

template <typename T>
Promise<T>::Promise() {
  shared_state_ = std::make_shared<SharedState>();
}

template <typename T>
Promise<T>::~Promise() {
  if (!shared_state_) {
    return;
  }

  std::lock_guard<std::mutex> lock(shared_state_->mutex);
  if (!shared_state_->result) {
    // Mark the shared state as being cancelled.
    shared_state_->result = CancelledFuture{};
    shared_state_->ready_condition.notify_all();
  }
}

template <typename T>
void Promise<T>::set(T&& x) {
  std::lock_guard<std::mutex> lock(shared_state_->mutex);
  assert(!shared_state_->result);  // Promise can only be set once.
  shared_state_->result.emplace(std::move(x));
  shared_state_->ready_condition.notify_all();
  for (auto watch : shared_state_->watches) {
    (*watch)(Future<T>::SharedState::GetCancelledOrSharedResult(shared_state_));
  }
  shared_state_->watches.clear();
}

template <typename T>
Future<T> Promise<T>::future() {
  return Future<T>(shared_state_);
}

template <typename T>
Future<T>::Watch::~Watch() {
  if (shared_state_) {
    std::lock_guard<std::mutex> lock(shared_state_->mutex);
    shared_state_->watches.erase(on_ready_.get());
  }
}

template <typename T>
Future<T>::Watch::Watch(
    const std::shared_ptr<SharedState>& shared_state,
    const std::function<void(const CancelledOrSharedResult&)>& on_ready)
    : shared_state_(shared_state),
      on_ready_(
          new std::function<void(const CancelledOrSharedResult&)>(on_ready)) {
  std::lock_guard<std::mutex> lock(shared_state_->mutex);
  if (shared_state_->result) {
    on_ready(Future<T>::SharedState::GetCancelledOrSharedResult(shared_state_));
  } else {
    shared_state_->watches.insert(on_ready_.get());
  }
}

template <typename T>
typename Future<T>::Watch Future<T>::watch(
    const std::function<void(const CancelledOrSharedResult&)>& on_ready) {
  auto x = Watch(shared_state_, on_ready);
  return x;
}

}  // namespace utils
}  // namespace reify

#endif  // _SRC_UTILS_INCLUDE_REIFY_UTILS_FUTURE_H_
