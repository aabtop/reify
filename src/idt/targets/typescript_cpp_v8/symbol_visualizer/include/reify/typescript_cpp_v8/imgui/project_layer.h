#ifndef _IDT_TARGETS_TYPESCRIPT_CPP_V8_SYMBOL_VISUALIZER_IMGUI_PROJECT_LAYER_H
#define _IDT_TARGETS_TYPESCRIPT_CPP_V8_SYMBOL_VISUALIZER_IMGUI_PROJECT_LAYER_H

#include <filesystem>
#include <map>
#include <optional>
#include <set>

#include "reify/typescript_cpp_v8/imgui/runtime_layer.h"
#include "reify/typescript_cpp_v8/typescript_cpp_v8.h"
#include "reify/utils/error.h"
#include "reify/utils/thread_with_work_queue.h"

namespace ImGui {
// We forward declare this class to avoid including `imfilebrowser.h`, which
// includes <windows.h>, which is huge.
class FileBrowser;
}  // namespace ImGui

namespace reify {
namespace typescript_cpp_v8 {
namespace imgui {

class ProjectLayer {
 public:
  ProjectLayer(
      utils::WorkQueue* self_work_queue, StatusLayer* status_layer,
      RuntimeLayer* runtime_layer,
      const std::optional<std::filesystem::path>& initial_project_path);
  ~ProjectLayer();

  void ExecuteImGuiCommands();

 private:
  void LoadProject(const std::filesystem::path& project_path);

  StatusLayer* status_layer_;
  RuntimeLayer* runtime_layer_;
  SymbolVisualizer* symbol_visualizer_;

  std::optional<StatusLayer::Window> status_window_;

  std::optional<HostFilesystemProjectWithBuildFilesGetter> project_;
  std::optional<CompilerEnvironmentThreadSafe::MultiCompileResults>
      compile_results_;

  utils::ScopedWorkQueue self_work_queue_;
  std::optional<CompilerEnvironmentThreadSafe::MultiCompileFuture::Watch>
      pending_compile_results_;

  std::unique_ptr<ImGui::FileBrowser> file_browser_;
};

}  // namespace imgui
}  // namespace typescript_cpp_v8
}  // namespace reify

#endif  // _IDT_TARGETS_TYPESCRIPT_CPP_V8_SYMBOL_VISUALIZER_IMGUI_PROJECT_LAYER_H