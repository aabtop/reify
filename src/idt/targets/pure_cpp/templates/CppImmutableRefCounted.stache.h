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
#include <type_traits>

{{#enable_hashes}}
#include "reify/pure_cpp/hashing.h"
{{/enable_hashes}}

namespace {{namespace}} {

{{#declarationSequence}}
{{{.}}}
{{/declarationSequence}}

}  // {{namespace}}

// This is the same regardless of domain, so we only want to define it once,
// but it's nice to avoid a header dependency if we can by putting the
// definition here.
#ifndef CPP_IMMUT_REF_COUNTED_IST_GENERATED_H_GENERIC_NEW
#define CPP_IMMUT_REF_COUNTED_IST_GENERATED_H_GENERIC_NEW
namespace reify {

template <typename T>
inline std::shared_ptr<const T> New(T&& x) {
  return std::make_shared<T>(std::move(x));
}

template <typename T>
using Reference = decltype(New(std::declval<std::decay_t<T>>()));

}  // namespace reify
#endif  // CPP_IMMUT_REF_COUNTED_IST_GENERATED_H_GENERIC_NEW

{{#enable_hashes}}
#include "reify/pure_cpp/hashing_post_definitions.h"
{{/enable_hashes}}


#endif  // _{{namespace}}_CPP_IMMUT_REF_COUNTED_IST_GENERATED_H_
