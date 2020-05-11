#ifndef TYPESCRIPT_COMPILER_TYPESCRIPT_COMPILER_H_
#define TYPESCRIPT_COMPILER_TYPESCRIPT_COMPILER_H_

#include <v8.h>

#include <string>
#include <variant>
#include <vector>

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
  struct CompileOptions {
    std::vector<Module> system_modules;
  };

  struct TranspileResults {
    std::string primary_module;
    std::vector<Module> modules;
    const Module* LookupPath(const std::string& path) const;
    const Module& GetPrimaryModule() const {
      return *LookupPath(primary_module);
    }
  };
  struct Error {
    std::string path;
    int line;
    int column;
    std::string message;
  };
  std::variant<TranspileResults, Error> TranspileToJavaScript(
      const char* input_path, const char* input_typescript,
      const CompileOptions& options = CompileOptions());

 private:
  v8::Isolate::CreateParams isolate_create_params_;
  v8::Isolate* isolate_;
  v8::Persistent<v8::Context> context_;

  v8::Persistent<v8::Function> transpile_function_;
};

#endif  // TYPESCRIPT_COMPILER_TYPESCRIPT_COMPILER_H_
