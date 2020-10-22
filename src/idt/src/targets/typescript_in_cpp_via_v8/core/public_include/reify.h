#ifndef _REIFY_REIFY_H_
#define _REIFY_REIFY_H_

#include <cassert>
#include <filesystem>
#include <functional>
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

// Allows specification of how to find and load imported TypeScript modules.
class VirtualFilesystem {
 public:
  struct Error {
    std::string message;
  };
  struct FilePath {
    FilePath(
        const std::string& diagnostics_path,
        const std::function<bool()>& exists,
        const std::function<std::variant<Error, std::string>()>& get_content)
        : diagnostics_path(diagnostics_path),
          exists(exists),
          get_content(get_content) {}

    // The path that will represent this file for the sake of diagnostics,
    // for example this is the path that will be displayed to the user when
    // an error occurs.  By letting this differ from the virtual file path,
    // we enable error messages to be meaningful to users while the scripts
    // continue to be sandboxed.
    std::string diagnostics_path;
    // Returns true if the file exists, otherwise returns false.
    std::function<bool()> exists;
    // A function that, when called, will return the contents of the file.
    std::function<std::variant<Error, std::string>()> get_content;
  };

  // Returns a handle to a file, given a virtual file path.  The virtual
  // file system is the one in which the TypeScript compiler will operate.
  // Virtual absolute paths always start with a `/`.
  virtual FilePath GetPath(std::string_view virtual_absolute_path) = 0;
};

// Similar to `chroot`, this VirtualFilesystem implementation will create a
// virtual filesystem with the root set as a folder in the host filesystem.
class MountedHostFolderFilesystem : public VirtualFilesystem {
 public:
  MountedHostFolderFilesystem(const std::filesystem::path& host_root);
  FilePath GetPath(std::string_view virtual_absolute_path) override;

  // Converts a host path into a virtual path.  If the host path is not
  // contained within the mounted folder, a std::nullopt is returend.
  std::optional<std::string> HostPathToVirtualPath(
      const std::filesystem::path& host_absolute_path);

 private:
  std::filesystem::path TranslateToHostPath(
      std::string_view virtual_absolute_path) const;
  std::filesystem::path host_root_;
};

// Entire filesystem is specified up-front via a filepath to content mapping.
class InMemoryFilesystem : public VirtualFilesystem {
 public:
  using FileMap = std::unordered_map<std::string, std::string>;
  InMemoryFilesystem(const FileMap& file_map);
  FilePath GetPath(std::string_view virtual_absolute_path) override;

 private:
  FileMap file_map_;
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

  struct ExportedSymbol {
    // The name of the exported symbol.
    std::string name;

    // A string returned by the TypeScript compiler which represents the
    // exported symbol's type.
    std::string typescript_type_string;

    template <typename T>
    bool HasType() const {
      return hypo_v8::TypeMatchesTypeScriptString<T>::Result(
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
  enum class SnapshotOptions {
    // Do we create and cache a snapshot of the TypeScript
    // compiler to improve efficiency of startup time?
    kCacheSnapshot,
    // Never create or load snapshots of the TypeScript
    // compiler.
    kNoSnapshot,
  };

  CompilerEnvironment(
      VirtualFilesystem* virtual_filesystem,
      SnapshotOptions snapshot_options = SnapshotOptions::kNoSnapshot);
  CompilerEnvironment(const CompilerEnvironment&) = delete;
  CompilerEnvironment(CompilerEnvironment&&) = delete;
  ~CompilerEnvironment();

  std::variant<CompileError, std::shared_ptr<CompiledModule>> Compile(
      std::string_view virtual_absolute_path);

  // Creates a directory at the specified path containing the root of a
  // TypeScript project setup to recognize the Reify types.  For example,
  // it will create a `tsconfig.json` file as well as a directory containing
  // the `.d.ts` TypeScript declaration files.
  // Returns true on success and false on failure.
  static bool CreateWorkspaceDirectory(
      const std::filesystem::path& out_dir_path);

 private:
  class Impl;
  std::unique_ptr<Impl> impl_;
};

}  // namespace reify
}  // namespace REIFY_GENERATED_PROJECT_NAMESPACE

#define WITH_V8_(x) x##_v8
#define WITH_V8(x) WITH_V8_(x)

namespace WITH_V8(REIFY_GENERATED_PROJECT_NAMESPACE) {
  template <typename R>
  struct TypeMatchesTypeScriptString<
      REIFY_GENERATED_PROJECT_NAMESPACE::reify::Function<R()>> {
    static bool Result(std::string_view ts) {
      const std::string_view kParameterlessSignature("() => ");
      if (ts.substr(0, kParameterlessSignature.size()) !=
          kParameterlessSignature) {
        return false;
      }
      std::string_view return_value(ts);
      return_value.remove_prefix(kParameterlessSignature.size());
      return hypo_v8::TypeMatchesTypeScriptString<R>::Result(return_value);
    }
  };
}  // namespace )

#endif  // _REIFY_REIFY_H_
