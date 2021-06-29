#ifndef _REIFY_COMPILED_MODULE_IMPL_H_
#define _REIFY_COMPILED_MODULE_IMPL_H_

#include "public_include/reify/typescript_cpp_v8/typescript_cpp_v8.h"
#include "typescript_compiler.h"

namespace reify {
namespace typescript_cpp_v8 {

class CompiledModule::Impl {
 public:
  Impl(TypeScriptCompiler::TranspileResults&& transpile_results)
      : transpile_results_(std::move(transpile_results)){};

  const TypeScriptCompiler::TranspileResults& transpile_results() const {
    return transpile_results_;
  }

 private:
  TypeScriptCompiler::TranspileResults transpile_results_;
};

}  // namespace typescript_cpp_v8
}  // namespace reify

#endif  // _REIFY_COMPILED_MODULE_IMPL_H_
