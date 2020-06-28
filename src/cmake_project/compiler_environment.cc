#include <fstream>
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

  bool CreateWorkspaceDirectory(const std::filesystem::path& out_dir_path);

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

bool CompilerEnvironment::CreateWorkspaceDirectory(
    const std::filesystem::path& out_dir_path) {
  return impl_->CreateWorkspaceDirectory(out_dir_path);
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

namespace {
bool WriteToFile(const std::filesystem::path& path, std::string_view content) {
  std::ofstream fout(path);
  if (fout.fail()) {
    std::cerr << "Error opening " << path << " for writing." << std::endl;
    return false;
  }
  fout.write(content.data(), content.length());
  if (fout.fail()) {
    std::cerr << "Error writing data to " << path << std::endl;
    return false;
  }
  return true;
}

const char* TSCONFIG_JSON_CONTENT = R"json(
{
  "compilerOptions": {
    "target": "ES2015",
    "module": "ES2015",
    "lib": [],
    "strict": true,
    "baseUrl": "./declarations",
  },
}
)json";

}  // namespace

bool CompilerEnvironment::Impl::CreateWorkspaceDirectory(
    const std::filesystem::path& out_dir_path) {
  auto transpile_results_or_error =
      Compile(LIB_INTERFACE_MODULE.path, LIB_INTERFACE_MODULE.content,
              GenerateDeclarationFiles::Yes);
  if (auto error =
          std::get_if<TypeScriptCompiler::Error>(&transpile_results_or_error)) {
    // We shouldn't have any errors here since we're just compiling a
    // TypeScript file generated by us.
    assert(false);
  }

  std::filesystem::create_directories(out_dir_path);
  if (!std::filesystem::directory_entry(out_dir_path).exists()) {
    std::cerr << "Error creating directory " << out_dir_path << std::endl;
    return false;
  }
  const auto declarations_directory = out_dir_path / "declarations";
  std::filesystem::create_directories(declarations_directory);
  if (!std::filesystem::directory_entry(declarations_directory).exists()) {
    std::cerr << "Error creating declarations directory "
              << declarations_directory << std::endl;
    return false;
  }

  auto transpile_results = std::get<TypeScriptCompiler::TranspileResults>(
      transpile_results_or_error);

  for (auto& declaration : transpile_results.declaration_files) {
    if (!WriteToFile(declaration.path, declaration.content)) {
      return false;
    }
  }

  if (!WriteToFile(out_dir_path / "tsconfig.json", TSCONFIG_JSON_CONTENT)) {
    return false;
  }

  return true;
}

}  // namespace reify
}  // namespace REIFY_GENERATED_PROJECT_NAMESPACE
