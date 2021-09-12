#ifndef _REIFY_IDT_TARGETS_PURE_CPP_HASHING_H_
#define _REIFY_IDT_TARGETS_PURE_CPP_HASHING_H_

#include <memory>

#include "blake3.h"

namespace reify {

template <typename T>
inline std::shared_ptr<const T> New(T&& x);

inline void AddObjectToHash(blake3_hasher* hasher, int input) {
  blake3_hasher_update(hasher, reinterpret_cast<const uint8_t*>(&input),
                       sizeof(input));
}

inline void AddObjectToHash(blake3_hasher* hasher, float input) {
  blake3_hasher_update(hasher, reinterpret_cast<const uint8_t*>(&input),
                       sizeof(input));
}

template <typename... U>
inline void AddObjectToHash(blake3_hasher* hasher,
                            const std::variant<U...>& input) {
  size_t variant_index = input.index();
  blake3_hasher_update(hasher, reinterpret_cast<const uint8_t*>(&variant_index),
                       sizeof(variant_index));

  std::visit([hasher](const auto& arg) { AddObjectToHash(hasher, arg); },
             input);
}

inline void AddObjectToHash(blake3_hasher* hasher, const std::string& input) {
  blake3_hasher_update(hasher, reinterpret_cast<const uint8_t*>(input.data()),
                       input.size());
}

template <typename T, unsigned long N>
inline void AddObjectToHash(blake3_hasher* hasher,
                            const std::array<T, N>& input) {
  for (const auto& i : input) {
    AddObjectToHash(hasher, i);
  }
}

template <typename T>
inline void AddObjectToHash(blake3_hasher* hasher,
                            const std::vector<T>& input) {
  for (const auto& i : input) {
    AddObjectToHash(hasher, i);
  }
}

template <typename T>
inline void AddObjectToHash(blake3_hasher* hasher,
                            const std::shared_ptr<const T>& input) {
  blake3_hasher_update(hasher, reinterpret_cast<const uint8_t*>(&input->hash),
                       sizeof(input->hash));
}

template <size_t Index = 0, typename... U>
inline typename std::enable_if<Index == sizeof...(U), void>::type
AddObjectToHash(blake3_hasher* hasher, const std::tuple<U...>& input) {}

template <size_t Index = 0, typename... U>
    inline typename std::enable_if <
    Index<sizeof...(U), void>::type AddObjectToHash(
        blake3_hasher* hasher, const std::tuple<U...>& input) {
  AddObjectToHash(hasher, std::get<Index>(input));
  AddObjectToHash<hasher, Index + 1, U...>(input);
}

template <typename T>
class CachedHashReference {
 public:
  using BaseType = std::decay_t<T>;

  CachedHashReference(const CachedHashReference& x)
      : ptr_(x.ptr_), hash_(x.hash_) {}
  CachedHashReference(CachedHashReference&& x)
      : ptr_(std::move(x.ptr_)), hash_(x.hash_) {}
  CachedHashReference(BaseType&& x);

  operator std::shared_ptr<const BaseType>() const { return ptr_; }
  const BaseType& operator*() const { return *ptr_; }
  const BaseType* operator->() const { return ptr_.operator->(); }

  uint64_t hash() const { return hash_; }

 private:
  std::shared_ptr<const BaseType> ptr_;
  uint64_t hash_;
};

template <typename T>
inline void AddObjectToHash(blake3_hasher* hasher,
                            const CachedHashReference<T>& input) {
  auto hash = input.hash();
  blake3_hasher_update(hasher, reinterpret_cast<const uint8_t*>(&hash),
                       sizeof(hash));
}

template <typename T>
inline uint64_t HashObject(const T& input);

}  // namespace reify

#endif  // _REIFY_IDT_TARGETS_PURE_CPP_HASHING_H_
