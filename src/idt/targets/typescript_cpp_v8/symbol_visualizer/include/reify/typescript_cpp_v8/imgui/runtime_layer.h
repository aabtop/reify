#ifndef _IDT_TARGETS_TYPESCRIPT_CPP_V8_SYMBOL_VISUALIZER_IMGUI_RUNTIME_LAYER_H
#define _IDT_TARGETS_TYPESCRIPT_CPP_V8_SYMBOL_VISUALIZER_IMGUI_RUNTIME_LAYER_H

#include <map>
#include <optional>

#include "reify/pure_cpp/thread_pool_cache_runner.h"
#include "reify/typescript_cpp_v8/imgui/docking_layer.h"
#include "reify/typescript_cpp_v8/imgui/status_layer.h"
#include "reify/typescript_cpp_v8/symbol_visualizer.h"
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
      SymbolVisualizer* symbol_visualizer,
      const pure_cpp::ThreadPoolCacheRunner& thread_pool_cache_runner);

  void ExecuteImGuiCommands();

  SymbolVisualizer* symbol_visualizer() const { return symbol_visualizer_; }

  void SetCompileResults(
      const CompilerEnvironmentThreadSafe::MultiCompileResults&
          compile_results);

  bool build_active() const { return !!pending_preview_results_; };

 private:
  struct SymbolTreeNode : public std::map<std::string, SymbolTreeNode> {};
  struct PreviewableModuleEntry {
    VirtualFilesystem::AbsolutePath module_path;
    std::shared_ptr<CompiledModule> module;
    std::vector<CompiledModule::ExportedSymbol> symbols;
  };
  struct SelectedSymbol {
    PreviewableModuleEntry previewable_entry;
    size_t symbol_preview_index;
  };
  using PreviewableSymbols =
      std::map<VirtualFilesystem::AbsolutePath, PreviewableModuleEntry>;

  static PreviewableSymbols ComputePreviewableSymbolsFromCompileResults(
      const CompilerEnvironmentThreadSafe::MultiCompileResults& compile_results,
      const std::function<bool(CompiledModule::ExportedSymbol)>& can_preview);
  static SymbolTreeNode VirtualPathSetToComponentTrie(
      const std::set<VirtualFilesystem::AbsolutePath>& paths);
  void RebuildSelectedSymbol();
  void RenderSymbolTree(const std::vector<std::string>& previous_components,
                        const SymbolTreeNode& tree);

  DockingLayer* docking_layer_;
  StatusLayer* status_layer_;
  SymbolVisualizer* symbol_visualizer_;
  const pure_cpp::ThreadPoolCacheRunner& thread_pool_cache_runner_;

  CompilerEnvironmentThreadSafe::MultiCompileResults compile_results_;

  std::optional<utils::Error> preview_error_;
  // Is there a symbol currently being previewed in the symbol visualizer?
  bool preview_active = false;

  PreviewableSymbols previewable_symbols_;

  std::optional<SelectedSymbol> selected_symbol_;

  std::optional<StatusLayer::Window> status_window_;

  // This needs to be last so that it is the first to be destructed.
  utils::ScopedWorkQueue self_work_queue_;

  std::optional<utils::Future<utils::ErrorOr<std::any>>::Watch>
      pending_preview_results_;

  // Newly appearing windows will take focus in the dock tab stack, on the
  // next frame after they are first introduced or re-introduced, but we
  // don't want that for the visualizer windows.  Instead, keep track of if
  // we're about to bring a window up, and if so, we'll explicitly keep the
  // focus on the symbol select tab on the next frame.
  bool visualizer_window_newly_opened_ = false;
};

}  // namespace imgui
}  // namespace typescript_cpp_v8
}  // namespace reify

#endif  // _IDT_TARGETS_TYPESCRIPT_CPP_V8_SYMBOL_VISUALIZER_IMGUI_RUNTIME_LAYER_H
