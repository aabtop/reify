#ifndef _REIFY_COMPILED_MODULE_IMPL_H_
#define _REIFY_COMPILED_MODULE_IMPL_H_

#include "public_include/reify.h"
#include "typescript_compiler.h"

namespace REIFY_GENERATED_PROJECT_NAMESPACE {
namespace reify {

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

}  // namespace reify
}  // namespace REIFY_GENERATED_PROJECT_NAMESPACE

#endif  // _REIFY_COMPILED_MODULE_IMPL_H_
