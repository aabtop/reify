#ifndef _IDT_TARGETS_TYPESCRIPT_CPP_V8_DOMAIN_VISUALIZER_IMGUI_PROJECT_LAYER_H
#define _IDT_TARGETS_TYPESCRIPT_CPP_V8_DOMAIN_VISUALIZER_IMGUI_PROJECT_LAYER_H

#include <filesystem>
#include <optional>

#include "reify/typescript_cpp_v8.h"
#include "reify/typescript_cpp_v8/imgui/runtime_layer.h"
#include "reify/utils/error.h"
#include "reify/utils/thread_with_work_queue.h"

namespace reify {
namespace typescript_cpp_v8 {
namespace imgui {

class ProjectLayer {
 public:
  ProjectLayer(
      utils::ThreadWithWorkQueue* thread, StatusLayer* status_layer,
      RuntimeLayer* runtime_layer,
      const std::optional<std::filesystem::path>& initial_project_path);

  void ExecuteImGuiCommands();

 private:
  void LoadProject(const std::filesystem::path& project_path);

  // The thread that this class's methods will be called on.
  utils::ThreadWithWorkQueue* thread_;
  utils::ThreadWithWorkQueue compilation_thread_;

  StatusLayer* status_layer_;
  RuntimeLayer* runtime_layer_;
  DomainVisualizer* domain_visualizer_;

  std::optional<std::filesystem::path> current_project_path_;
  std::optional<utils::ErrorOr<std::shared_ptr<reify::CompiledModule>>>
      compilation_results_;
  std::optional<utils::Future<
      utils::ErrorOr<std::shared_ptr<reify::CompiledModule>>>::Watch>
      pending_compilation_results_;

  std::optional<StatusLayer::Window> status_window_;
};

}  // namespace imgui
}  // namespace typescript_cpp_v8
}  // namespace reify

#endif  // _IDT_TARGETS_TYPESCRIPT_CPP_V8_DOMAIN_VISUALIZER_IMGUI_PROJECT_LAYER_H
