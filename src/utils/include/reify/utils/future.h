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

 private:
  struct SharedState {
    std::mutex mutex;
    std::condition_variable ready_condition;
    std::unordered_set<Watch*> watches;

    std::optional<CancelledOrResult> result;
  };

 public:
  class Watch {
   public:
    ~Watch();

   private:
    template <typename U>
    friend class Promise;
    friend class Future<T>;

    Watch(const std::shared_ptr<SharedState>& shared_state,
          const std::function<void(const CancelledOrResult&)>& on_ready);

    std::shared_ptr<SharedState> shared_state_;
    std::function<void(const CancelledOrResult&)> on_ready_;
  };

  Watch watch(const std::function<void(const CancelledOrResult&)>& on_ready);

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
Promise<T>::Promise() {
  shared_state_ = std::make_shared<SharedState>();
}

template <typename T>
Promise<T>::~Promise() {
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
    watch->on_ready_(*shared_state_->result);
  }
  shared_state_->watches.clear();
}

template <typename T>
Future<T> Promise<T>::future() {
  return Future<T>(shared_state_);
}

template <typename T>
Future<T>::Watch::~Watch() {
  std::lock_guard<std::mutex> lock(shared_state_->mutex);
  shared_state_->watches.erase(this);
}

template <typename T>
Future<T>::Watch::Watch(
    const std::shared_ptr<SharedState>& shared_state,
    const std::function<void(const CancelledOrResult&)>& on_ready)
    : shared_state_(shared_state), on_ready_(on_ready) {
  std::lock_guard<std::mutex> lock(shared_state_->mutex);
  if (shared_state_->result) {
    on_ready(*shared_state_->result);
  } else {
    shared_state_->watches.insert(this);
  }
}

template <typename T>
typename Future<T>::Watch Future<T>::watch(
    const std::function<void(const CancelledOrResult&)>& on_ready) {
  return Watch(shared_state_, on_ready);
}

}  // namespace utils
}  // namespace reify