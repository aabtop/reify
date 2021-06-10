#ifndef _IDT_TARGETS_TYPESCRIPT_CPP_V8_DOMAIN_VISUALIZER_IMGUI_PROJECT_LAYER_H
#define _IDT_TARGETS_TYPESCRIPT_CPP_V8_DOMAIN_VISUALIZER_IMGUI_PROJECT_LAYER_H

#include <filesystem>
#include <map>
#include <optional>
#include <set>

#include "reify/typescript_cpp_v8.h"
#include "reify/typescript_cpp_v8/imgui/runtime_layer.h"
#include "reify/utils/error.h"
#include "reify/utils/thread_with_work_queue.h"

namespace reify {
namespace typescript_cpp_v8 {
namespace imgui {

class CompilerEnvironmentThreadSafe {
 public:
  using CompilationResults =
      std::variant<CompileError, std::shared_ptr<CompiledModule>>;
  using CompilationFuture = utils::Future<CompilationResults>;
  CompilerEnvironmentThreadSafe(
      VirtualFilesystem* virtual_filesystem,
      const std::vector<reify::CompilerEnvironment::InputModule>&
          typescript_input_modules);
  ~CompilerEnvironmentThreadSafe();

  // Invalidates the stored cache for the compiled results of the specified
  // sources.  It is important to call this whenever inputs change, otherwise
  // the compiler environment will just used cached copies of input files.
  void InvalidateCompiledResultsCache(const std::set<std::string>& sources);

  // Compile the specified TypeScript source files into JavaScript output.
  CompilationFuture Compile(const std::string& sources);

 private:
  VirtualFilesystem* virtual_filesystem_;
  const std::vector<reify::CompilerEnvironment::InputModule>
      typescript_input_modules_;
  std::optional<CompilerEnvironment> compiler_environment_;
  utils::ThreadWithWorkQueue compilation_thread_;
};

class Project {
 public:
  const std::filesystem::path& absolute_path() const { return absolute_path_; }

  CompilerEnvironmentThreadSafe::CompilationFuture RebuildProject();

 private:
  friend utils::ErrorOr<std::unique_ptr<Project>> CreateProjectFromPath(
      const std::filesystem::path& path,
      const std::vector<reify::CompilerEnvironment::InputModule>&
          typescript_input_modules);

  Project(const std::filesystem::path& absolute_path,
          std::unique_ptr<VirtualFilesystem> virtual_filesystem,
          const std::vector<reify::CompilerEnvironment::InputModule>&
              typescript_input_modules,
          const std::function<std::string()>& get_sources);

  const std::filesystem::path absolute_path_;
  std::unique_ptr<VirtualFilesystem> virtual_filesystem_;
  const std::function<std::string()> get_sources_;
  CompilerEnvironmentThreadSafe compiler_environment_;
};

utils::ErrorOr<std::unique_ptr<Project>> CreateProjectFromPath(
    const std::filesystem::path& path,
    const std::vector<reify::CompilerEnvironment::InputModule>&
        typescript_input_modules);

class ProjectLayer {
 public:
  ProjectLayer(
      utils::WorkQueue* self_work_queue, StatusLayer* status_layer,
      RuntimeLayer* runtime_layer,
      const std::optional<std::filesystem::path>& initial_project_path);

  void ExecuteImGuiCommands();

 private:
  void LoadProject(const std::filesystem::path& project_path);

  StatusLayer* status_layer_;
  RuntimeLayer* runtime_layer_;
  DomainVisualizer* domain_visualizer_;

  std::unique_ptr<Project> project_;
  std::optional<
      utils::ErrorOr<CompilerEnvironmentThreadSafe::CompilationResults>>
      compilation_results_;
  std::optional<CompilerEnvironmentThreadSafe::CompilationFuture::Watch>
      pending_compilation_results_;

  std::optional<StatusLayer::Window> status_window_;
  utils::ScopedWorkQueue self_work_queue_;
};

}  // namespace imgui
}  // namespace typescript_cpp_v8
}  // namespace reify

#endif  // _IDT_TARGETS_TYPESCRIPT_CPP_V8_DOMAIN_VISUALIZER_IMGUI_PROJECT_LAYER_H
