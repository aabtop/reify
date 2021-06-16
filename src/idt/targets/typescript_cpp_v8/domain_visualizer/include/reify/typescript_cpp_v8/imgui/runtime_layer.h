#ifndef _IDT_TARGETS_TYPESCRIPT_CPP_V8_DOMAIN_VISUALIZER_IMGUI_RUNTIME_LAYER_H
#define _IDT_TARGETS_TYPESCRIPT_CPP_V8_DOMAIN_VISUALIZER_IMGUI_RUNTIME_LAYER_H

#include <map>
#include <optional>

#include "reify/typescript_cpp_v8/domain_visualizer.h"
#include "reify/typescript_cpp_v8/imgui/docking_layer.h"
#include "reify/typescript_cpp_v8/imgui/status_layer.h"
#include "reify/typescript_cpp_v8/typescript_cpp_v8.h"
#include "reify/utils/error.h"
#include "reify/utils/thread_with_work_queue.h"

namespace reify {
namespace typescript_cpp_v8 {
namespace imgui {

class RuntimeLayer {
 public:
  RuntimeLayer(
      const std::function<void(std::function<void()>)>& enqueue_task_function,
      DockingLayer* docking_layer, StatusLayer* status_layer,
      DomainVisualizer* domain_visualizer);

  void ExecuteImGuiCommands();

  DomainVisualizer* domain_visualizer() const { return domain_visualizer_; }

  void SetCompileResults(
      const std::map<
          std::string,
          std::variant<CompileError, std::shared_ptr<CompiledModule>>>&
          compile_results);

  bool build_active() const { return !!pending_preview_results_; };

 private:
  struct ExportedSymbolWithSourceFile {
    // This may potentially include the source file as well, if multiple
    // source files are involved.
    std::string display_name;
    std::shared_ptr<CompiledModule> module;
    CompiledModule::ExportedSymbol symbol;
  };
  static std::vector<ExportedSymbolWithSourceFile>
  ComputePreviewableSymbolsFromCompileResults(
      const std::map<
          std::string,
          std::variant<CompileError, std::shared_ptr<CompiledModule>>>&
          compile_results,
      const std::function<bool(CompiledModule::ExportedSymbol)>& can_preview);
  void RebuildSelectedSymbol();

  DockingLayer* docking_layer_;
  StatusLayer* status_layer_;
  DomainVisualizer* domain_visualizer_;
  std::map<std::string,
           std::variant<CompileError, std::shared_ptr<CompiledModule>>>
      compile_results_;

  std::optional<utils::Error> preview_error_;

  std::vector<ExportedSymbolWithSourceFile> previewable_symbols_;

  int selected_symbol_index_ = -1;
  std::optional<std::string> selected_symbol_name_;

  std::optional<StatusLayer::Window> status_window_;

  // This needs to be last so that it is the first to be destructed.
  utils::ScopedWorkQueue self_work_queue_;

  std::optional<utils::Future<
      DomainVisualizer::ErrorOr<DomainVisualizer::PreparedSymbol>>::Watch>
      pending_preview_results_;
};

}  // namespace imgui
}  // namespace typescript_cpp_v8
}  // namespace reify

#endif  // _IDT_TARGETS_TYPESCRIPT_CPP_V8_DOMAIN_VISUALIZER_IMGUI_RUNTIME_LAYER_H
