#include <fstream>
#include <iostream>
#include <string_view>

#include "compiled_module_impl.h"
#include "public_include/reify/typescript_cpp_v8.h"
#include "typescript_compiler.h"

namespace reify {

class CompilerEnvironment::Impl {
 public:
  Impl(VirtualFilesystem* virtual_filesystem,
       const std::vector<InputModule>* initial_modules,
       SnapshotOptions snapshot_cache_options)
      : initial_modules_(initial_modules),
        tsc_(virtual_filesystem,
             snapshot_cache_options == SnapshotOptions::kNoSnapshot
                 ? TypeScriptCompiler::SnapshotOptions::kNoSnapshot
                 : TypeScriptCompiler::SnapshotOptions::kCacheSnapshot) {}

  enum class GenerateDeclarationFiles { Yes, No };
  std::variant<TypeScriptCompiler::TranspileResults, TypeScriptCompiler::Error>
  Compile(std::string_view virtual_absolute_path,
          GenerateDeclarationFiles generate_declaration_files);

 private:
  const std::vector<InputModule>* initial_modules_;
  TypeScriptCompiler tsc_;
};

CompilerEnvironment::CompilerEnvironment(
    VirtualFilesystem* virtual_filesystem,
    const std::vector<InputModule>* initial_modules,
    SnapshotOptions snapshot_options)
    : impl_(new CompilerEnvironment::Impl(virtual_filesystem, initial_modules,
                                          snapshot_options)) {}
CompilerEnvironment::~CompilerEnvironment() {}

CompilerEnvironment::CompileResults CompilerEnvironment::Compile(
    std::string_view virtual_absolute_path) {
  auto transpile_results_or_error =
      impl_->Compile(virtual_absolute_path, Impl::GenerateDeclarationFiles::No);
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
CompilerEnvironment::Impl::Compile(
    std::string_view virtual_absolute_path,
    GenerateDeclarationFiles generate_declaration_files) {
  return tsc_.TranspileToJavaScript(
      virtual_absolute_path,
      {*initial_modules_,
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
    "baseUrl": "./",
  },
}
)json";

}  // namespace

CompilerEnvironment::CompilerEnvironment(CompilerEnvironment&& x)
    : impl_(std::move(x.impl_)) {}

bool CompilerEnvironment::CreateWorkspaceDirectory(
    const std::filesystem::path& out_dir_path,
    const std::vector<InputModule>& initial_modules) {
  InMemoryFilesystem filesystem(
      InMemoryFilesystem::FileMap({{std::string(initial_modules[0].path),
                                    std::string(initial_modules[0].content)}}));
  CompilerEnvironment compiler_environment(&filesystem, &initial_modules);
  auto transpile_results_or_error = compiler_environment.impl_->Compile(
      initial_modules[0].path, Impl::GenerateDeclarationFiles::Yes);
  if (std::holds_alternative<TypeScriptCompiler::Error>(
          transpile_results_or_error)) {
    // We shouldn't have any errors here since we're just compiling a
    // TypeScript file generated by us.
    assert(false);
  }

  std::filesystem::create_directories(out_dir_path);
  if (!std::filesystem::directory_entry(out_dir_path).exists()) {
    std::cerr << "Error creating directory " << out_dir_path << std::endl;
    return false;
  }
  const auto declarations_directory = out_dir_path;
  std::filesystem::create_directories(declarations_directory);
  if (!std::filesystem::directory_entry(declarations_directory).exists()) {
    std::cerr << "Error creating declarations directory "
              << declarations_directory << std::endl;
    return false;
  }

  auto transpile_results = std::get<TypeScriptCompiler::TranspileResults>(
      transpile_results_or_error);

  for (auto& declaration : transpile_results.declaration_files) {
    if (!WriteToFile(declarations_directory / declaration.path,
                     declaration.content)) {
      return false;
    }
  }

  if (!WriteToFile(out_dir_path / "tsconfig.json", TSCONFIG_JSON_CONTENT)) {
    return false;
  }

  return true;
}

CompilerEnvironmentThreadSafe::MultiCompileFuture Project::RebuildProject() {
  return compiler_environment_.MultiCompile(get_sources_());
}

CompilerEnvironmentThreadSafe::CompilerEnvironmentThreadSafe(
    VirtualFilesystem* virtual_filesystem,
    const std::vector<reify::CompilerEnvironment::InputModule>&
        typescript_input_modules)
    : virtual_filesystem_(std::move(virtual_filesystem)),
      typescript_input_modules_(typescript_input_modules) {
  compilation_thread_.Enqueue([this] {
    compiler_environment_.emplace(virtual_filesystem_,
                                  &typescript_input_modules_);
  });
}

CompilerEnvironmentThreadSafe::~CompilerEnvironmentThreadSafe() {
  compilation_thread_.Enqueue([this] { compiler_environment_ = std::nullopt; });
}

CompilerEnvironmentThreadSafe::CompileFuture
CompilerEnvironmentThreadSafe::Compile(const std::string& sources) {
  return compilation_thread_.EnqueueWithResult<CompileResults>(
      [this, sources] { return compiler_environment_->Compile(sources); });
}

CompilerEnvironmentThreadSafe::MultiCompileFuture
CompilerEnvironmentThreadSafe::MultiCompile(
    const std::set<std::string>& sources) {
  return compilation_thread_.EnqueueWithResult<MultiCompileResults>(
      [this, sources] {
        MultiCompileResults results;
        for (const auto& source : sources) {
          results[source] = compiler_environment_->Compile(source);
        }
        return results;
      });
}

}  // namespace reify
