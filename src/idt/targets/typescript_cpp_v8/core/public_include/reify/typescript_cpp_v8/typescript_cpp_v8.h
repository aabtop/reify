#ifndef _REIFY_TYPESCRIPT_CPP_V8_H_
#define _REIFY_TYPESCRIPT_CPP_V8_H_

#include <cassert>
#include <filesystem>
#include <functional>
#include <map>
#include <memory>
#include <optional>
#include <set>
#include <string_view>
#include <unordered_map>
#include <variant>

// We need to include V8 in this header file in order to translate project
// interface template types into V8 template types.
#include <v8.h>

#include "reify/typescript_cpp_v8/common_types.h"
#include "reify/typescript_cpp_v8/virtual_filesystem.h"
#include "reify/utils/error.h"
#include "reify/utils/future.h"
#include "reify/utils/thread_with_work_queue.h"

namespace reify {
namespace typescript_cpp_v8 {

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

  friend struct CallContext;
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

    return reify_v8::Value<typename reify_v8::FromImmRefCnt<R>::type>::Call(
        call_context.isolate,
        v8::Local<typename reify_v8::FromImmRefCnt<R>::type>::Cast(
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
  RuntimeEnvironment(RuntimeEnvironment&&);
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

  struct ExportedSymbol {
    // The name of the exported symbol.
    std::string name;

    // A string returned by the TypeScript compiler which represents the
    // exported symbol's type.
    std::string typescript_type_string;

    template <typename T>
    bool HasType() const {
      return reify_v8::TypeMatchesTypeScriptString<T>::Result(
          typescript_type_string);
    }
  };
  const std::vector<ExportedSymbol>& exported_symbols() const;
  const ExportedSymbol* GetExportedSymbol(
      std::string_view export_symbol_name) const;

  class Impl;
  Impl* impl() { return impl_.get(); }

 private:
  CompiledModule(std::unique_ptr<Impl> impl);

  const std::unique_ptr<Impl> impl_;

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
  enum class SnapshotOptions {
    // Do we create and cache a snapshot of the TypeScript
    // compiler to improve efficiency of startup time?
    kCacheSnapshot,
    // Never create or load snapshots of the TypeScript
    // compiler.
    kNoSnapshot,
  };

  struct InputModule {
    std::string_view path;
    std::string_view content;
  };

  using CompileResults =
      std::variant<CompileError, std::shared_ptr<CompiledModule>>;

  CompilerEnvironment(
      VirtualFilesystem* virtual_filesystem,
      const std::vector<InputModule>* initial_modules,
      SnapshotOptions snapshot_options = SnapshotOptions::kNoSnapshot);
  CompilerEnvironment(CompilerEnvironment&&);
  CompilerEnvironment(const CompilerEnvironment&) = delete;
  ~CompilerEnvironment();

  CompileResults Compile(std::string_view virtual_absolute_path);

  // Creates a directory at the specified path containing the root of a
  // TypeScript project setup to recognize the Reify types.  For example,
  // it will create a `tsconfig.json` file as well as a directory containing
  // the `.d.ts` TypeScript declaration files.
  // Returns true on success and false on failure.
  static bool CreateWorkspaceDirectory(
      const std::filesystem::path& out_dir_path,
      const std::vector<InputModule>& initial_modules);

 private:
  class Impl;
  std::unique_ptr<Impl> impl_;
};

// Similar to CompilerEnvironment above, but this one is thread safe, because
// it creates a thread dedicated to the compiler environment and shuttles
// inputs/outputs back and forth from that thread.
class CompilerEnvironmentThreadSafe {
 public:
  using CompileResults = CompilerEnvironment::CompileResults;
  using CompileFuture = utils::Future<CompileResults>;
  using MultiCompileResults = std::map<std::string, CompileResults>;
  using MultiCompileFuture = utils::Future<MultiCompileResults>;
  CompilerEnvironmentThreadSafe(
      VirtualFilesystem* virtual_filesystem,
      const std::vector<CompilerEnvironment::InputModule>&
          typescript_input_modules);
  ~CompilerEnvironmentThreadSafe();

  // Invalidates the stored cache for the compiled results of the specified
  // sources.  It is important to call this whenever inputs change, otherwise
  // the compiler environment will just used cached copies of input files.
  void InvalidateCompiledResultsCache(const std::set<std::string>& sources);

  // Compile the specified TypeScript source files into JavaScript output.
  CompileFuture Compile(const std::string& sources);

  // Compile multiple source files in one shot.
  MultiCompileFuture MultiCompile(const std::set<std::string>& sources);

 private:
  VirtualFilesystem* virtual_filesystem_;
  const std::vector<CompilerEnvironment::InputModule> typescript_input_modules_;
  std::optional<CompilerEnvironment> compiler_environment_;
  utils::ThreadWithWorkQueue compilation_thread_;
};

// Wraps a directory on the filesystem, treating it as a project directory
// within which all recursively discovered TypeScript ".ts" files will be
// compiled. Can persist and notify its owner whenever the files under the
// directory are modified, indicating that previously compiled results will be
// out of date.
class Project {
 public:
  // Calling CreateProjectFromPath() is the easiest way to create a project,
  // but using this constructor directly can provide more flexibility.
  Project(const std::filesystem::path& absolute_path,
          std::unique_ptr<VirtualFilesystem> virtual_filesystem,
          const std::vector<CompilerEnvironment::InputModule>&
              typescript_input_modules,
          const std::function<std::set<std::string>()>& get_sources);

  const std::filesystem::path& absolute_path() const { return absolute_path_; }

  CompilerEnvironmentThreadSafe::MultiCompileFuture RebuildProject();

 private:
  const std::filesystem::path absolute_path_;
  std::unique_ptr<VirtualFilesystem> virtual_filesystem_;
  const std::function<std::set<std::string>()> get_sources_;
  CompilerEnvironmentThreadSafe compiler_environment_;
};

// Given a specified input path, will derive from it a project configuration.
// If the provided input path is a single file, then only a single file will
// be visible and compiled, however if a folder is provided instead, all
// contents within the folder will be compiled and visible.
utils::ErrorOr<std::unique_ptr<Project>> CreateProjectFromPath(
    const std::filesystem::path& path,
    const std::vector<CompilerEnvironment::InputModule>&
        typescript_input_modules);

}  // namespace typescript_cpp_v8
}  // namespace reify

namespace reify_v8 {
template <typename R>
struct TypeMatchesTypeScriptString<reify::typescript_cpp_v8::Function<R()>> {
  static bool Result(std::string_view ts) {
    const std::string_view kParameterlessSignature("() => ");
    if (ts.substr(0, kParameterlessSignature.size()) !=
        kParameterlessSignature) {
      return false;
    }
    std::string_view return_value(ts);
    return_value.remove_prefix(kParameterlessSignature.size());
    return reify_v8::TypeMatchesTypeScriptString<R>::Result(return_value);
  }
};

}  // namespace reify_v8

#endif  // _REIFY_TYPESCRIPT_CPP_V8_H_
