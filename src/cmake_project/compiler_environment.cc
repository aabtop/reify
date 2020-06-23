#include <iostream>
#include <string_view>

#include "compiled_module_impl.h"
#include "public_include/reify.h"
#include "typescript_compiler.h"

#define xstr(s) str(s)
#define str(s) #s

namespace {
#include "src_gen/lib_ts.h"
#include "src_gen/reify_generated_interface_ts.h"
}  // namespace

#define TS_LIB_TS_FROM_NAMESPACE2(x) ts_##x##_ts
#define TS_LIB_TS_FROM_NAMESPACE(x) TS_LIB_TS_FROM_NAMESPACE2(x)
#define TS_LIB_TS_LEN_FROM_NAMESPACE2(x) ts_##x##_ts_len
#define TS_LIB_TS_LEN_FROM_NAMESPACE(x) TS_LIB_TS_LEN_FROM_NAMESPACE2(x)

#define TS_LIB_TS TS_LIB_TS_FROM_NAMESPACE(REIFY_GENERATED_PROJECT_NAMESPACE)
#define TS_LIB_TS_LEN \
  TS_LIB_TS_LEN_FROM_NAMESPACE(REIFY_GENERATED_PROJECT_NAMESPACE)

namespace REIFY_GENERATED_PROJECT_NAMESPACE {
namespace reify {

class CompilerEnvironment::Impl {
 public:
  Impl(SnapshotOptions snapshot_cache_options)
      : tsc_(snapshot_cache_options == SnapshotOptions::kNoSnapshot
                 ? TypeScriptCompiler::SnapshotOptions::kNoSnapshot
                 : TypeScriptCompiler::SnapshotOptions::kCacheSnapshot) {}

  std::variant<TypeScriptCompiler::TranspileResults, TypeScriptCompiler::Error>
  Compile(std::string_view path, std::string_view source);

 private:
  TypeScriptCompiler tsc_;
};

CompilerEnvironment::CompilerEnvironment(SnapshotOptions snapshot_options)
    : impl_(new CompilerEnvironment::Impl(snapshot_options)) {}
CompilerEnvironment::~CompilerEnvironment() {}

std::variant<CompileError, std::shared_ptr<CompiledModule>>
CompilerEnvironment::Compile(std::string_view path, std::string_view source) {
  auto transpile_results_or_error = impl_->Compile(path, source);
  if (auto error =
          std::get_if<TypeScriptCompiler::Error>(&transpile_results_or_error)) {
    return *error;
  }
  const auto& results = std::get<TypeScriptCompiler::TranspileResults>(
      transpile_results_or_error);
  for (const auto& declaration_file : results.declaration_files) {
    std::cout << std::endl;
    std::cout << "declaration file path: " << declaration_file.path << std::endl
              << std::endl;
    std::cout << declaration_file.content << std::endl;
    std::cout << std::endl << std::endl << std::endl;
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
      reinterpret_cast<const char*>(ts_reify_generated_interface_ts),
      ts_reify_generated_interface_ts_len);
  std::string_view ts_lib_reify_str(reinterpret_cast<const char*>(TS_LIB_TS),
                                    TS_LIB_TS_LEN);

  return tsc_.TranspileToJavaScript(
      path, source,
      {.system_modules = {
           {"/reify_generated_interface.ts", reify_ts_interface_src_str},
           {"/" xstr(REIFY_GENERATED_PROJECT_NAMESPACE) ".ts",
            ts_lib_reify_str}}});
}

}  // namespace reify
}  // namespace REIFY_GENERATED_PROJECT_NAMESPACE
