#ifndef _SRC_UTILS_INCLUDE_REIFY_UTILS_CIRCULAR_QUEUE_H_
#define _SRC_UTILS_INCLUDE_REIFY_UTILS_CIRCULAR_QUEUE_H_

#include <array>
#include <cassert>
#include <optional>

namespace reify {
namespace utils {

template <typename T, size_t N>
class CircularQueue {
 public:
  size_t size() const {
    return (back_ == front_
                ? (data_[back_] ? N : 0)
                : (back_ > front_ ? back_ - front_ : N - front_ + back_));
  }
  bool empty() const { return size() == 0; }
  bool full() const { return size() == N; }

  void enqueue(const T& x) {
    assert(!full());
    *next_free_data() = x;
  }
  void enqueue(T&& x) {
    assert(!full());
    *next_free_data() = std::move(x);
  }

  const T& peek() const {
    assert(!empty());
    return *data_[front_];
  }

  T dequeue() {
    assert(!empty());
    T output = std::move(*data_[front_]);
    data_[front_] = std::nullopt;
    front_ = increment_circular(front_);
    return std::move(output);
  }

 private:
  static size_t increment_circular(size_t x) { return (x + 1) % N; }
  std::optional<T>* next_free_data() {
    assert(!full());
    size_t index = back_;
    back_ = increment_circular(back_);
    return &(data_[index]);
  }

  // This could be stored more efficiently without using `std::optional`, but
  // this is easier.
  std::array<std::optional<T>, N> data_;
  size_t front_ = 0;
  size_t back_ = 0;
};

}  // namespace utils
}  // namespace reify

#endif  // _SRC_UTILS_INCLUDE_REIFY_UTILS_CIRCULAR_QUEUE_H_
