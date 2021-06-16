#ifndef TYPESCRIPT_COMPILER_TYPESCRIPT_COMPILER_H_
#define TYPESCRIPT_COMPILER_TYPESCRIPT_COMPILER_H_

#include <v8.h>

#include <string>
#include <string_view>
#include <variant>
#include <vector>

#include "global_initialization.h"
#include "public_include/reify/typescript_cpp_v8/typescript_cpp_v8.h"

namespace reify {
namespace typescript_cpp_v8 {

class TypeScriptCompiler {
 public:
  enum class SnapshotOptions {
    kCacheSnapshot,
    kNoSnapshot,
  };

  using ExportedSymbol = CompiledModule::ExportedSymbol;

  // Used to represent both input TypeScript modules and output JavaScript
  // ES2015 modules.
  struct Module {
    std::string path;
    std::string content;
  };

  using InputModule = CompilerEnvironment::InputModule;

  struct CompileOptions {
    std::vector<InputModule> system_modules;
    bool generate_declaration_files;
  };

  struct TranspileResults {
    std::string primary_module;
    std::vector<Module> modules;
    std::vector<Module> declaration_files;

    // Symbols exported from the primary module.
    std::vector<ExportedSymbol> exported_symbols;

    // VFS used during compilation of this module.
    VirtualFilesystem* virtual_filesystem;

    const Module* LookupPath(const std::string& path) const;
    const Module& GetPrimaryModule() const {
      return *LookupPath(primary_module);
    }
  };

  using Error = CompileError;

  TypeScriptCompiler(
      VirtualFilesystem* virtual_filesystem,
      SnapshotOptions snapshot_options = SnapshotOptions::kNoSnapshot);
  ~TypeScriptCompiler();

  std::variant<TranspileResults, Error> TranspileToJavaScript(
      std::string_view virtual_absolute_path,
      const CompileOptions& options = CompileOptions());

 private:
  GlobalV8InitializationEnsurer global_v8_initialization_ensurer_;

  bool LoadIsolateFromSnapshot();
  bool CreateAndSaveIsolateToSnapshot();
  v8::Local<v8::Context> InitializeIsolate(
      v8::Isolate* isolate, v8::SnapshotCreator* snapshot_creator);
  void LocateTranspileFunction();
  void InitializeIsolateWithoutSnapshot();

  VirtualFilesystem* virtual_filesystem_;

  v8::Isolate::CreateParams isolate_create_params_;
  v8::Isolate* isolate_;
  v8::Persistent<v8::Context> context_;

  v8::Persistent<v8::Function> transpile_function_;
};

}  // namespace typescript_cpp_v8
}  // namespace reify

#endif  // TYPESCRIPT_COMPILER_TYPESCRIPT_COMPILER_H_
