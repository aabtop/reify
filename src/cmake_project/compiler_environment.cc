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

  enum class GenerateDeclarationFiles { Yes, No };
  std::variant<TypeScriptCompiler::TranspileResults, TypeScriptCompiler::Error>
  Compile(std::string_view path, std::string_view source,
          GenerateDeclarationFiles generate_declaration_files);

  std::vector<DeclarationFile> GetDeclarationFiles();

 private:
  TypeScriptCompiler tsc_;
};

CompilerEnvironment::CompilerEnvironment(SnapshotOptions snapshot_options)
    : impl_(new CompilerEnvironment::Impl(snapshot_options)) {}
CompilerEnvironment::~CompilerEnvironment() {}

std::variant<CompileError, std::shared_ptr<CompiledModule>>
CompilerEnvironment::Compile(std::string_view path, std::string_view source) {
  auto transpile_results_or_error =
      impl_->Compile(path, source, Impl::GenerateDeclarationFiles::No);
  if (auto error =
          std::get_if<TypeScriptCompiler::Error>(&transpile_results_or_error)) {
    return *error;
  }

  return std::shared_ptr<CompiledModule>(
      new CompiledModule(std::make_unique<CompiledModule::Impl>(
          std::move(std::get<TypeScriptCompiler::TranspileResults>(
              transpile_results_or_error)))));
}

std::vector<CompilerEnvironment::DeclarationFile>
CompilerEnvironment::GetDeclarationFiles() {
  return impl_->GetDeclarationFiles();
}

namespace {
const TypeScriptCompiler::InputModule REIFY_GENERATED_INTERFACE_MODULE = {
    "/reify_generated_interface.ts",
    std::string_view(
        reinterpret_cast<const char*>(ts_reify_generated_interface_ts),
        ts_reify_generated_interface_ts_len)};
const TypeScriptCompiler::InputModule LIB_INTERFACE_MODULE = {
    "/" xstr(REIFY_GENERATED_PROJECT_NAMESPACE) ".ts",
    std::string_view(reinterpret_cast<const char*>(TS_LIB_TS), TS_LIB_TS_LEN)};

}  // namespace

std::variant<TypeScriptCompiler::TranspileResults, TypeScriptCompiler::Error>
CompilerEnvironment::Impl::Compile(
    std::string_view path, std::string_view source,
    GenerateDeclarationFiles generate_declaration_files) {
  return tsc_.TranspileToJavaScript(
      path, source,
      {.system_modules = {REIFY_GENERATED_INTERFACE_MODULE,
                          LIB_INTERFACE_MODULE},
       .generate_declaration_files =
           (generate_declaration_files == GenerateDeclarationFiles::Yes)});
}

std::vector<CompilerEnvironment::DeclarationFile>
CompilerEnvironment::Impl::GetDeclarationFiles() {
  auto transpile_results_or_error =
      Compile(LIB_INTERFACE_MODULE.path, LIB_INTERFACE_MODULE.content,
              GenerateDeclarationFiles::Yes);
  if (auto error =
          std::get_if<TypeScriptCompiler::Error>(&transpile_results_or_error)) {
    assert(false);
  }

  auto transpile_results = std::get<TypeScriptCompiler::TranspileResults>(
      transpile_results_or_error);

  std::vector<DeclarationFile> declarations;
  declarations.reserve(transpile_results.declaration_files.size());
  for (auto& declaration : transpile_results.declaration_files) {
    declarations.push_back({.filepath = std::move(declaration.path),
                            .content = std::move(declaration.content)});
  }
  return declarations;
}

}  // namespace reify
}  // namespace REIFY_GENERATED_PROJECT_NAMESPACE
