// {{!
// clang-format off
// }}
#ifndef _{{namespace}}_CPP_V8_IST_GENERATED_H_
#define _{{namespace}}_CPP_V8_IST_GENERATED_H_

#include <cassert>
#include <tuple>
#include <vector>

#include <v8.h>

#include "reify/typescript_cpp_v8/common_types.h"
#include "reify/typescript_cpp_v8/typescript_cpp_v8.h"

#include "reify/purecpp/{{immutableRefCountedHeaderFile}}"

namespace reify_v8 {

{{#declarationSequence}}
{{{.}}}
{{/declarationSequence}}

}  // namespace reify_v8

namespace reify {
namespace typescript_cpp_v8 {
namespace {{immRefCntNamespace}} {

std::vector<CompilerEnvironment::InputModule> typescript_declarations();

}  // namespace {{immRefCntNamespace}}
}  // namespace typescript_cpp_v8
}  // namespace reify

#endif  // _{{namespace}}_CPP_V8_IST_GENERATED_H_