#ifndef _IDT_TARGETS_TYPESCRIPT_CPP_V8_DOMAIN_VISUALIZER_IMGUI_RUNTIME_LAYER_H
#define _IDT_TARGETS_TYPESCRIPT_CPP_V8_DOMAIN_VISUALIZER_IMGUI_RUNTIME_LAYER_H

#include <optional>

#include "reify/typescript_cpp_v8.h"
#include "reify/typescript_cpp_v8/domain_visualizer.h"
#include "reify/utils/error.h"

namespace reify {
namespace typescript_cpp_v8 {
namespace imgui {

class RuntimeLayer {
 public:
  RuntimeLayer(
      const std::function<void(std::function<void()>)>& enqueue_task_function,
      DomainVisualizer* domain_visualizer);

  void ExecuteImGuiCommands();

  DomainVisualizer* domain_visualizer() const { return domain_visualizer_; }

  void SetCompiledModule(
      const std::shared_ptr<reify::CompiledModule>& compiled_module);

 private:
  std::function<void(std::function<void()>)> enqueue_task_function_;
  DomainVisualizer* domain_visualizer_;
  std::shared_ptr<reify::CompiledModule> compiled_module_;

  std::optional<utils::Error> preview_error_;

  std::vector<reify::CompiledModule::ExportedSymbol> previewable_symbols_;

  int selected_symbol_index_ = -1;
};

}  // namespace imgui
}  // namespace typescript_cpp_v8
}  // namespace reify

#endif  // _IDT_TARGETS_TYPESCRIPT_CPP_V8_DOMAIN_VISUALIZER_IMGUI_RUNTIME_LAYER_H
