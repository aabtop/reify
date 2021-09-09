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
#include <iostream>

#include "reify/pure_cpp/common_types.h"

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


inline void AddObjectToHash(blake3_hasher* hasher, int input) {
  blake3_hasher_update(hasher, 
      reinterpret_cast<const uint8_t*>(&input), sizeof(input));
}

inline void AddObjectToHash(blake3_hasher* hasher, float input) {
  blake3_hasher_update(hasher, 
      reinterpret_cast<const uint8_t*>(&input), sizeof(input));
}


template<typename... U>
inline void AddObjectToHash(blake3_hasher* hasher, const std::variant<U...>& input) {
  size_t variant_index = input.index();
  blake3_hasher_update(hasher, 
    reinterpret_cast<const uint8_t*>(&variant_index), sizeof(variant_index));

  std::visit([hasher](const auto& arg) {
    AddObjectToHash(hasher, arg);    
  }, input);
}

inline void AddObjectToHash(blake3_hasher* hasher, const std::string& input) {
  blake3_hasher_update(hasher, 
      reinterpret_cast<const uint8_t*>(input.data()), input.size());
}

template <typename T, unsigned long N>
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
inline void AddObjectToHash(
    blake3_hasher* hasher, const std::shared_ptr<const T>& input) {
  blake3_hasher_update(hasher, 
      reinterpret_cast<const uint8_t*>(&input->hash), sizeof(input->hash));
}

template <typename T>
inline uint64_t HashObject(const std::shared_ptr<const T>& input) {
  std::cerr << "yo!";
  return input->hash;
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

{{/enable_hashes}}

{{#declarationSequence}}
{{{.}}}
{{/declarationSequence}}

}  // {{namespace}}

#endif  // _{{namespace}}_CPP_IMMUT_REF_COUNTED_IST_GENERATED_H_
