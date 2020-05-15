#ifndef TYPESCRIPT_COMPILER_TYPESCRIPT_COMPILER_H_
#define TYPESCRIPT_COMPILER_TYPESCRIPT_COMPILER_H_

#include <v8.h>

#include <string>
#include <string_view>
#include <variant>
#include <vector>

#include "global_initialization.h"
#include "public_include/reify.h"

namespace REIFY_GENERATED_PROJECT_NAMESPACE {
namespace reify {

class TypeScriptCompiler {
 public:
  TypeScriptCompiler();
  ~TypeScriptCompiler();

  // Used to represent both input TypeScript modules and output JavaScript
  // ES2015 modules.
  struct Module {
    std::string path;
    std::string content;
  };

  struct InputModule {
    std::string_view path;
    std::string_view content;
  };

  struct CompileOptions {
    std::vector<InputModule> system_modules;
  };

  struct TranspileResults {
    std::string primary_module;
    std::vector<Module> modules;
    const Module* LookupPath(const std::string& path) const;
    const Module& GetPrimaryModule() const {
      return *LookupPath(primary_module);
    }
  };

  using Error = CompileError;

  std::variant<TranspileResults, Error> TranspileToJavaScript(
      std::string_view input_path, std::string_view input_typescript,
      const CompileOptions& options = CompileOptions());

 private:
  GlobalV8InitializationEnsurer global_v8_initialization_ensurer_;

  v8::Isolate::CreateParams isolate_create_params_;
  v8::Isolate* isolate_;
  v8::Persistent<v8::Context> context_;

  v8::Persistent<v8::Function> transpile_function_;
};

}  // namespace reify
}  // namespace REIFY_GENERATED_PROJECT_NAMESPACE

#endif  // TYPESCRIPT_COMPILER_TYPESCRIPT_COMPILER_H_