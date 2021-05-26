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
  previewable_symbols_.clear();
  for (const auto& symbol : compiled_module_->exported_symbols()) {
    if (domain_visualizer_->CanPreviewSymbol(symbol)) {
      previewable_symbols_.push_back(symbol);
    }
  }
  if (!compiled_module_) {
    preview_error_ = std::nullopt;
    // TODO: Reset the current existing domain_visualizer_ preview.
  }
}

void RuntimeLayer::ExecuteImGuiCommands() {
  ImGui::Begin("Symbols");

  if (compiled_module_) {
    std::vector<const char*> symbols;
    symbols.reserve(previewable_symbols_.size());
    for (const auto& symbol : previewable_symbols_) {
      symbols.push_back(symbol.name.c_str());
    }

    if (ImGui::ListBox("Preview symbol", &selected_symbol_index_,
                       symbols.data(), symbols.size(), 6)) {
      if (selected_symbol_index_ >= 0) {
        domain_visualizer_->PrepareSymbolForPreview(
            compiled_module_, previewable_symbols_[selected_symbol_index_],
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

  ImGui::End();
}

}  // namespace imgui
}  // namespace typescript_cpp_v8
}  // namespace reify
