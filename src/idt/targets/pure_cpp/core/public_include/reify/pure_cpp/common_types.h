#ifndef _REIFY_IDT_TARGETS_PURE_CPP_COMMON_TYPES_H_
#define _REIFY_IDT_TARGETS_PURE_CPP_COMMON_TYPES_H_

namespace reify {

template <typename T>
class Traits {
 public:
  using Reference = std::shared_ptr<const T>;

  static inline Reference New(T&& x) {
    return std::make_shared<T>(std::move(x));
  }
};

template <typename T>
using Reference = typename Traits<std::decay_t<T>>::Reference;

template <typename T>
inline Reference<T> New(T&& x) {
  return Traits<T>::New(std::move(x));
}

template <typename T>
class TraitsFromReference {};

template <typename T>
class TraitsFromReference<typename Traits<std::decay_t<T>>::Reference> {
 public:
  using type = Traits<std::decay_t<T>>;
};

template <typename T>
inline const T& Deref(const std::shared_ptr<T>& x) {
  return *x;
}

}  // namespace reify

#endif  // _REIFY_IDT_TARGETS_PURE_CPP_COMMON_TYPES_H_
