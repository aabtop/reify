#include <string_view>

#include "compiled_module_impl.h"
#include "public_include/reify.h"
#include "typescript_compiler.h"

#define xstr(s) str(s)
#define str(s) #s

namespace {
#include "src_gen/reify_generated_interface_ts.h"
#include "src_gen/reify_interface_ts.h"
}  // namespace

namespace REIFY_GENERATED_PROJECT_NAMESPACE {
namespace reify {

class CompilerEnvironment::Impl {
 public:
  Impl(){};

  std::variant<TypeScriptCompiler::TranspileResults, TypeScriptCompiler::Error>
  Compile(std::string_view path, std::string_view source);

 private:
  TypeScriptCompiler tsc_;
};

CompilerEnvironment::CompilerEnvironment()
    : impl_(new CompilerEnvironment::Impl()) {}
CompilerEnvironment::~CompilerEnvironment() {}

std::variant<CompileError, std::shared_ptr<CompiledModule>>
CompilerEnvironment::Compile(std::string_view path, std::string_view source) {
  auto transpile_results_or_error = impl_->Compile(path, source);
  if (auto error =
          std::get_if<TypeScriptCompiler::Error>(&transpile_results_or_error)) {
    return *error;
  }

  return std::shared_ptr<CompiledModule>(
      new CompiledModule(std::make_unique<CompiledModule::Impl>(
          std::move(std::get<TypeScriptCompiler::TranspileResults>(
              transpile_results_or_error)))));
}

std::variant<TypeScriptCompiler::TranspileResults, TypeScriptCompiler::Error>
CompilerEnvironment::Impl::Compile(std::string_view path,
                                   std::string_view source) {
  std::string_view reify_ts_interface_src_str(
      reinterpret_cast<const char*>(ts_reify_ts_interface_ts),
      ts_reify_ts_interface_ts_len);
  std::string_view ts_lib_reify_str(reinterpret_cast<const char*>(ts_lib_ts),
                                    ts_lib_ts_len);

  return tsc_.TranspileToJavaScript(
      path, source,
      {.system_modules = {
           {"/reify_generated_interface.ts", reify_ts_interface_src_str},
           {"/" xstr(REIFY_GENERATED_PROJECT_NAMESPACE) ".ts",
            ts_lib_reify_str}}});
}

}  // namespace reify
}  // namespace REIFY_GENERATED_PROJECT_NAMESPACE
