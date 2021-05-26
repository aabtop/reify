#include "reify/typescript_cpp_v8/imgui/runtime_layer.h"

#include "imgui.h"

namespace reify {
namespace typescript_cpp_v8 {
namespace imgui {

RuntimeLayer::RuntimeLayer(
    const std::function<void(std::function<void()>)>& enqueue_task_function,
    DomainVisualizer* domain_visualizer)
    : domain_visualizer_(domain_visualizer),
      enqueue_task_function_(enqueue_task_function) {}

void RuntimeLayer::SetCompiledModule(
    const std::shared_ptr<reify::CompiledModule>& compiled_module) {
  compiled_module_ = compiled_module;
  if (!compiled_module_) {
    preview_error_ = std::nullopt;
    // TODO: Reset the current existing domain_visualizer_ preview.
    return;
  }

  for (auto symbol : compiled_module->exported_symbols()) {
    if (domain_visualizer_->CanPreviewSymbol(symbol)) {
      domain_visualizer_->PrepareSymbolForPreview(
          compiled_module, symbol,
          [this, enqueue_task_function = enqueue_task_function_](
              DomainVisualizer::ErrorOr<DomainVisualizer::PreparedSymbol> x) {
            enqueue_task_function([this, x] {
              if (auto error = std::get_if<0>(&x)) {
                preview_error_ = utils::Error{error->msg};
              }
              domain_visualizer_->Preview(std::get<1>(x));
            });
          });
    }
  }
}

void RuntimeLayer::ExecuteImGuiCommands() {
  ImGui::Begin("I AM A RUNTIME LAYER!!!");

  ImGui::End();
}

}  // namespace imgui
}  // namespace typescript_cpp_v8
}  // namespace reify
