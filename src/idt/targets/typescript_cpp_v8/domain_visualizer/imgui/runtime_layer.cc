#include "reify/typescript_cpp_v8/imgui/runtime_layer.h"

#include <fmt/format.h>

#include "imgui.h"
#include "reify/typescript_cpp_v8/imgui/widgets.h"

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
  if (previewable_symbols_.empty()) {
    selected_symbol_index_ = -1;
    selected_symbol_name_ = std::nullopt;
  } else {
    selected_symbol_index_ = 0;
    for (size_t i = 0; i < previewable_symbols_.size(); ++i) {
      if (selected_symbol_name_ == previewable_symbols_[i].name) {
        selected_symbol_index_ = static_cast<int>(i);
        break;
      }
    }
    selected_symbol_name_ = previewable_symbols_[selected_symbol_index_].name;
  }
  RebuildSelectedSymbol();
}

void RuntimeLayer::ExecuteImGuiCommands() {
  ImGui::PushStyleVar(ImGuiStyleVar_WindowMinSize, ImVec2(300, 250));

  ImGui::Begin("Runtime");

  if (compiled_module_) {
    std::vector<const char*> symbols;
    symbols.reserve(previewable_symbols_.size());
    for (const auto& symbol : previewable_symbols_) {
      symbols.push_back(symbol.name.c_str());
    }

    if (ImGui::ListBox("Preview symbol", &selected_symbol_index_,
                       symbols.data(), symbols.size(), 6)) {
      selected_symbol_name_ = previewable_symbols_[selected_symbol_index_].name;
      RebuildSelectedSymbol();
    }

    if (build_active()) {
      Spinner("building spinner", 10.0f, ImVec4{0.2, 0.6, 0.5, 1.0},
              ImVec4{0.1, 0.3, 0.2, 1.0}, 10, 2.5f);
      ImGui::SameLine();
      ImGui::Text(fmt::format("Building {}...",
                              previewable_symbols_[selected_symbol_index_].name)
                      .c_str());
    }
  }

  ImGui::End();

  ImGui::PopStyleVar();
}

void RuntimeLayer::RebuildSelectedSymbol() {
  if (pending_preview_results_) {
    // Don't rebuild if we're already rebuilding.
    return;
  }

  if (selected_symbol_index_ >= 0) {
    pending_preview_results_ =
        domain_visualizer_
            ->PrepareSymbolForPreview(
                compiled_module_, previewable_symbols_[selected_symbol_index_])
            .watch(
                [this, enqueue_task_function = enqueue_task_function_](auto x) {
                  enqueue_task_function([this, x] {
                    pending_preview_results_ = std::nullopt;

                    if (std::holds_alternative<utils::CancelledFuture>(x)) {
                      preview_error_ = utils::Error{"Compilation cancelled."};
                      return;
                    }

                    const auto& error_or = *std::get<1>(x);
                    if (auto error = std::get_if<0>(&error_or)) {
                      preview_error_ = utils::Error{error->msg};
                    }
                    domain_visualizer_->SetPreview(std::get<1>(error_or));
                  });
                });
  }
}

}  // namespace imgui
}  // namespace typescript_cpp_v8
}  // namespace reify
