// {{!
// clang-format off
// }}
#include "{{v8HeaderFile}}"

#include <cassert>
#include <v8.h>

#include "{{immutableRefCountedHeaderFile}}"
#include "context_environment.h"

namespace reify_v8 {

using namespace {{namespace}};

{{#convertToImmRefCntFunctions}}
{{{.}}}
{{/convertToImmRefCntFunctions}}

}  // namespace reify_v8

namespace {
#include "src_gen/lib_ts.h"
#include "src_gen/reify_generated_interface_ts.h"
}  // namespace

namespace reify {
namespace typescript_cpp_v8 {
namespace {{immRefCntNamespace}} {

std::vector<CompilerEnvironment::InputModule> typescript_declarations() {
  const CompilerEnvironment::InputModule lib_interface_module = {
      *VirtualFilesystem::AbsolutePath::FromComponents({"{{immRefCntNamespace}}.ts"}),
      std::string_view(reinterpret_cast<const char*>(lib_ts), lib_ts_len)};

  const CompilerEnvironment::InputModule reify_generated_module = {
      *VirtualFilesystem::AbsolutePath::FromComponents({"reify_generated_interface.ts"}),
      std::string_view(
          reinterpret_cast<const char*>(reify_generated_interface_ts),
          reify_generated_interface_ts_len)};

  return std::vector<CompilerEnvironment::InputModule>({lib_interface_module, reify_generated_module});
}

}  // namespace {{immRefCntNamespace}}
}  // namespace typescript_cpp_v8
}  // namespace reify
