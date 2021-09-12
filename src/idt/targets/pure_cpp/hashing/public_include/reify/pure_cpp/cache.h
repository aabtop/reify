#ifndef _REIFY_IDT_TARGETS_PURE_CPP_CACHE_H_
#define _REIFY_IDT_TARGETS_PURE_CPP_CACHE_H_

#include "reify/pure_cpp/hashing.h"

namespace reify {
namespace pure_cpp {

template <typename R, typename T>
R ConstructMemoized(const reify::CachedHashReference<T>& x, R (*f)(const T&)) {
  static std::unordered_map<uint64_t, std::unique_ptr<R>> cache;
  auto found = cache.find(x.hash());
  if (found == cache.end()) {
    std::tie(found, std::ignore) =
        cache.insert({x.hash(), std::unique_ptr<R>(new R(f(*x)))});
  }
  return *found->second;
}

}  // namespace pure_cpp
}  // namespace reify

#endif  // _REIFY_IDT_TARGETS_PURE_CPP_CACHE_H_
