// {{!
// clang-format off
// }}
#ifndef _{{namespace}}_CPP_V8_IST_GENERATED_H_
#define _{{namespace}}_CPP_V8_IST_GENERATED_H_

#include <cassert>
#include <tuple>
#include <vector>

#include <v8.h>

#include "reify/common_types.h"
#include "reify/typescript_cpp_v8.h"

#include "{{immutableRefCountedHeaderFile}}"

namespace reify_v8 {

{{#declarationSequence}}
{{{.}}}
{{/declarationSequence}}

}  // namespace reify_v8

namespace reify {

std::vector<CompilerEnvironment::InputModule>
    {{namespace}}_typescript_declarations();

}  // namespace reify

#endif  // _{{namespace}}_CPP_V8_IST_GENERATED_H_