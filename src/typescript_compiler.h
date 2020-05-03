#ifndef TYPESCRIPT_COMPILER_TYPESCRIPT_COMPILER_H_
#define TYPESCRIPT_COMPILER_TYPESCRIPT_COMPILER_H_

#include <v8.h>

#include <string>

class TypeScriptCompiler {
 public:
  TypeScriptCompiler();
  ~TypeScriptCompiler();

  std::string TranspileToJavaScript(const char* input_typescript);

 private:
  v8::Isolate::CreateParams isolate_create_params_;
  v8::Isolate* isolate_;
  v8::Persistent<v8::Context> context_;

  v8::Persistent<v8::Function> transpile_function_;
};

#endif  // TYPESCRIPT_COMPILER_TYPESCRIPT_COMPILER_H_
