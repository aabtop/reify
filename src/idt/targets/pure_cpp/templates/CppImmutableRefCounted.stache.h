// {{!
// clang-format off
// }}
#ifndef _{{namespace}}_CPP_IMMUT_REF_COUNTED_IST_GENERATED_H_
#define _{{namespace}}_CPP_IMMUT_REF_COUNTED_IST_GENERATED_H_

#include <array>
#include <memory>
#include <string>
#include <tuple>
#include <variant>
#include <vector>

{{#enable_hashes}}
#include "blake3.h"
{{/enable_hashes}}

namespace {{namespace}} {

{{#enable_hashes}}
template <typename T>
inline uint64_t HashObject(const T& input) {
  blake3_hasher hasher;
  blake3_hasher_init(&hasher);

  AddObjectToHash(&hasher, input);

  uint64_t result;
  blake3_hasher_finalize(
      &hasher, reinterpret_cast<uint8_t*>(&result), sizeof(result));
  return result;
}

template <typename T>
inline void AddObjectToHash(blake3_hasher* hasher, const T& input) {
  static_assert(std::is_standard_layout<T>::value,
                "Unsupported type for hasher.");
  blake3_hasher_update(hasher, 
      reinterpret_cast<const uint8_t*>(&input), sizeof(input));
}

inline void AddObjectToHash(blake3_hasher* hasher, const std::string& input) {
  blake3_hasher_update(hasher, 
      reinterpret_cast<const uint8_t*>(input.data()), input.size());
}

template <typename T, int N>
inline void AddObjectToHash(blake3_hasher* hasher, const std::array<T, N>& input) {
  for (const auto& i : input) {
    AddObjectToHash(hasher, i);
  }
}

template <typename T>
inline void AddObjectToHash(blake3_hasher* hasher, const std::vector<T>& input) {
  for (const auto& i : input) {
    AddObjectToHash(hasher, i);
  }
}

template <typename T>
struct ObjectAndHash {
  T object;
  uint64_t hash;
};

template <typename T>
inline void AddObjectToHash(
    blake3_hasher* hasher, const std::shared_ptr<const T>& input) {
  blake3_hasher_update(hasher, 
      reinterpret_cast<const uint8_t*>(&input->hash), sizeof(input->hash));
}

template<size_t Index = 0, typename... U>
inline typename std::enable_if<Index == sizeof...(U), void>::type
AddObjectToHash(blake3_hasher* hasher, const std::tuple<U...>& input) {}

template<size_t Index = 0, typename... U>
inline typename std::enable_if<Index < sizeof...(U), void>::type
AddObjectToHash(blake3_hasher* hasher, const std::tuple<U...>& input) {
  AddObjectToHash(hasher, std::get<Index>(input));
  AddObjectToHash<hasher, Index + 1, U...>(input);
}

template <typename T>
inline std::shared_ptr<const T> New(T&& x) {
  // Cache the hash of the referenced object.
  auto hash = HashObject(x);
  return std::make_shared<ObjectAndHash<T>>({std::move(x), hash});
}
{{/enable_hashes}}

{{#no_enable_hashes}}
template <typename T>
inline std::shared_ptr<const T> New(T&& x) {
  return std::make_shared<T>(std::move(x));
}
{{/no_enable_hashes}}

{{#declarationSequence}}
{{{.}}}
{{/declarationSequence}}

}  // {{namespace}}

#endif  // _{{namespace}}_CPP_IMMUT_REF_COUNTED_IST_GENERATED_H_
