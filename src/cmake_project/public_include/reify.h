#ifndef _REIFY_REIFY_H_
#define _REIFY_REIFY_H_

#include <cassert>
#include <memory>
#include <optional>
#include <string_view>

#include "reify_cpp_v8_interface.h"
#include "reify_generated_project_namespace.h"

// We need to include V8 in this header file in order to translate project
// interface template types into V8 template types.
#include <v8.h>

namespace REIFY_GENERATED_PROJECT_NAMESPACE {
namespace reify {

using RuntimeException = std::string;

class GenericFunction {
 public:
  GenericFunction(GenericFunction&&);

  class Impl;

  ~GenericFunction();

  struct CallContext {
    CallContext(GenericFunction* function);

    v8::Isolate* isolate;
    v8::Isolate::Scope isolate_scope;
    v8::HandleScope handle_scope;
    v8::Local<v8::Context> context;
    v8::Context::Scope context_scope;
    v8::TryCatch try_catch;
    v8::Local<v8::Function> function;

    std::variant<RuntimeException, v8::Local<v8::Value>> Call(
        int argc, v8::Local<v8::Value> argv[]);
  };

  Impl* impl() { return impl_.get(); }

 private:
  GenericFunction(std::unique_ptr<Impl> impl);

  std::unique_ptr<Impl> impl_;

  friend class CallContext;
  friend class RuntimeEnvironment;
};

template <typename T>
class Function {};

// Coming soon: parameters!
template <typename R>
class Function<R()> {
 public:
  Function(GenericFunction&& generic_function)
      : generic_function_(std::move(generic_function)) {}

  std::variant<RuntimeException, R> Call() {
    GenericFunction::CallContext call_context(&generic_function_);
    auto result_or_error = call_context.Call(0, nullptr);
    if (auto error = std::get_if<0>(&result_or_error)) {
      return *error;
    }

    return hypo_v8::Value(
        call_context.isolate,
        v8::Local<typename hypo_v8::FromImmRefCnt<R>::type>::Cast(
            std::get<1>(result_or_error)));
  }

 private:
  GenericFunction generic_function_;
};

class CompiledModule;

using GetExportError = std::string;

class RuntimeEnvironment {
 public:
  RuntimeEnvironment(const RuntimeEnvironment&) = delete;
  RuntimeEnvironment(RuntimeEnvironment&&) = default;
  ~RuntimeEnvironment();

  template <typename T>
  std::variant<GetExportError, T> GetExport(std::string_view symbol_name) {
    auto function_or_error = GetGenericExport(symbol_name);
    if (auto error = std::get_if<0>(&function_or_error)) {
      return *error;
    }
    return T(std::move(std::get<1>(function_or_error)));
  }

 private:
  class Impl;
  RuntimeEnvironment(std::unique_ptr<Impl>&& impl);

  std::variant<GetExportError, GenericFunction> GetGenericExport(
      std::string_view symbol_name);

  std::unique_ptr<Impl> impl_;

  friend std::variant<RuntimeException, RuntimeEnvironment>
  CreateRuntimeEnvironment(std::shared_ptr<CompiledModule> module);
};

class CompiledModule {
 public:
  CompiledModule(const CompiledModule&) = delete;
  CompiledModule(CompiledModule&&) = delete;
  ~CompiledModule();

  class Impl;
  Impl* impl() { return impl_.get(); }

 private:
  CompiledModule(std::unique_ptr<Impl> impl);

  std::unique_ptr<Impl> impl_;

  friend class CompilerEnvironment;
};

std::variant<RuntimeException, RuntimeEnvironment> CreateRuntimeEnvironment(
    std::shared_ptr<CompiledModule> module);

struct CompileError {
  std::string path;
  int line;
  int column;
  std::string message;
};

class CompilerEnvironment {
 public:
  CompilerEnvironment();
  CompilerEnvironment(const CompilerEnvironment&) = delete;
  CompilerEnvironment(CompilerEnvironment&&) = delete;
  ~CompilerEnvironment();

  std::variant<CompileError, std::shared_ptr<CompiledModule>> Compile(
      std::string_view path, std::string_view source);

 private:
  class Impl;
  std::unique_ptr<Impl> impl_;
};

}  // namespace reify
}  // namespace REIFY_GENERATED_PROJECT_NAMESPACE

#endif  // _REIFY_REIFY_H_
