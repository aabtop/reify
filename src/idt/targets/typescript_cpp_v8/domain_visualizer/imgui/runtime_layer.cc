#include "reify/typescript_cpp_v8/imgui/runtime_layer.h"

#include <fmt/format.h>

#include "imgui.h"
#include "imgui_internal.h"
#include "reify/typescript_cpp_v8/imgui/utils.h"
#include "reify/typescript_cpp_v8/imgui/widgets.h"

namespace reify {
namespace typescript_cpp_v8 {
namespace imgui {

RuntimeLayer::RuntimeLayer(
    const std::function<void(std::function<void()>)>& enqueue_task_function,
    DockingLayer* docking_layer, StatusLayer* status_layer,
    DomainVisualizer* domain_visualizer)
    : docking_layer_(docking_layer),
      status_layer_(status_layer),
      domain_visualizer_(domain_visualizer),
      self_work_queue_(enqueue_task_function) {}

std::vector<RuntimeLayer::ExportedSymbolWithSourceFile>
RuntimeLayer::ComputePreviewableSymbolsFromCompileResults(
    const std::map<std::string,
                   std::variant<CompileError, std::shared_ptr<CompiledModule>>>&
        compile_results,
    const std::function<bool(CompiledModule::ExportedSymbol)>& can_preview) {
  std::vector<ExportedSymbolWithSourceFile> previewable_symbols;
  for (const auto& compile_result : compile_results) {
    if (std::holds_alternative<CompileError>(compile_result.second)) {
      // For now we just silently skip over errors.
      continue;
    }

    std::shared_ptr<CompiledModule> compiled_module =
        std::get<1>(compile_result.second);

    for (const auto& symbol : compiled_module->exported_symbols()) {
      if (can_preview(symbol)) {
        std::string symbol_display_name =
            compile_results.size() > 1
                ? fmt::format("{}:{}", compile_result.first, symbol.name)
                : symbol.name;
        previewable_symbols.push_back(
            {symbol_display_name, compiled_module, symbol});
      }
    }
  }
  return previewable_symbols;
}

void RuntimeLayer::SetCompileResults(
    const std::map<std::string,
                   std::variant<CompileError, std::shared_ptr<CompiledModule>>>&
        compile_results) {
  compile_results_ = compile_results;
  previewable_symbols_ = ComputePreviewableSymbolsFromCompileResults(
      compile_results_,
      [this](auto x) { return domain_visualizer_->CanPreviewSymbol(x); });

  if (previewable_symbols_.empty()) {
    selected_symbol_index_ = -1;
  } else {
    selected_symbol_index_ = 0;
    for (size_t i = 0; i < previewable_symbols_.size(); ++i) {
      if (selected_symbol_name_ == previewable_symbols_[i].display_name) {
        selected_symbol_index_ = static_cast<int>(i);
        break;
      }
    }
  }
  RebuildSelectedSymbol();
}

namespace {
bool CompileResultsContainErrors(
    const std::map<std::string,
                   std::variant<CompileError, std::shared_ptr<CompiledModule>>>&
        compile_results) {
  for (const auto& item : compile_results) {
    if (std::holds_alternative<CompileError>(item.second)) {
      return true;
    }
  }
  return false;
}
}  // namespace

void RuntimeLayer::ExecuteImGuiCommands() {
  // Place this window within the right dock ID by default.
  const char* SELECT_SYMBOL_WINDOW_NAME = "Select Symbol";
  if (!ImGui::FindWindowByName(SELECT_SYMBOL_WINDOW_NAME)) {
    // If this is the first time we're seeing this window, default it into
    // the docked content menu.
    ImGui::SetNextWindowDockID(docking_layer_->GetDockedContentNodeId());
  }

  ImGui::PushStyleVar(ImGuiStyleVar_WindowMinSize, ImVec2(400, 250));
  ImGui::Begin(SELECT_SYMBOL_WINDOW_NAME);

  if (!previewable_symbols_.empty()) {
    std::vector<const char*> symbols;
    symbols.reserve(previewable_symbols_.size());
    for (const auto& symbol : previewable_symbols_) {
      symbols.push_back(symbol.display_name.c_str());
    }

    {
      DisableIf disable_if_pending_preview_results(!!pending_preview_results_);
      ImGui::PushItemWidth(-1);
      if (ImGui::ListBox("##preview_symbol_selector", &selected_symbol_index_,
                         symbols.data(), symbols.size(), symbols.size())) {
        selected_symbol_name_ =
            previewable_symbols_[selected_symbol_index_].display_name;
        RebuildSelectedSymbol();
      }
      ImGui::PopItemWidth();
    }
  }
  ImGui::End();
  ImGui::PopStyleVar();

  if (CompileResultsContainErrors(compile_results_)) {
    const char* ERROR_COMPILER_ERROR_WINDOW_NAME = "Compile Errors";

    if (!ImGui::FindWindowByName(ERROR_COMPILER_ERROR_WINDOW_NAME)) {
      // If this is the first time we're seeing this window, default it into
      // the bottom dock slot.
      ImGui::SetNextWindowDockID(docking_layer_->GetDockedBottomNodeId());
    }

    ImGui::Begin(ERROR_COMPILER_ERROR_WINDOW_NAME);
    for (const auto& item : compile_results_) {
      if (auto error = std::get_if<CompileError>(&item.second)) {
        if (ImGui::CollapsingHeader(item.first.c_str(),
                                    ImGuiTreeNodeFlags_DefaultOpen)) {
          std::string error_message = fmt::format("{}({}): {}", error->path,
                                                  error->line, error->message);
          const float indent_amount = ImGui::GetFontSize() * 2;
          ImGui::Indent(indent_amount);
          if (ImGui::Button("Copy")) {
            ImGui::LogToClipboard();
            ImGui::LogText("%s", error_message.c_str());
            ImGui::LogFinish();
          }
          ImGui::SameLine();
          ImGui::TextWrapped("%s", error_message.c_str());
          ImGui::Unindent(indent_amount);
        }
      }
    }
    ImGui::End();
  }

  if (preview_error_) {
    const char* RUNTIME_ERROR_WINDOW_NAME = "Runtime Errors";

    if (!ImGui::FindWindowByName(RUNTIME_ERROR_WINDOW_NAME)) {
      // If this is the first time we're seeing this window, default it into
      // the bottom dock slot.
      ImGui::SetNextWindowDockID(docking_layer_->GetDockedBottomNodeId());
    }

    ImGui::Begin(RUNTIME_ERROR_WINDOW_NAME);
    if (ImGui::Button("Copy")) {
      ImGui::LogToClipboard();
      ImGui::LogText("%s", preview_error_->msg.c_str());
      ImGui::LogFinish();
    }
    ImGui::SameLine();
    ImGui::TextWrapped("%s", preview_error_->msg.c_str());
    ImGui::End();
  }

  if (build_active()) {
    if (!status_window_) {
      status_window_.emplace(status_layer_, [this] {
        Spinner("status building spinner", 10.0f, ImVec4{0.2, 0.6, 0.5, 1.0},
                ImVec4{0.1, 0.3, 0.2, 1.0}, 10, 2.5f);
        ImGui::SameLine();
        ImGui::Text(
            "Building %s...",
            previewable_symbols_[selected_symbol_index_].display_name.c_str());
      });
    }

  } else {
    status_window_ = std::nullopt;
  }
}

void RuntimeLayer::RebuildSelectedSymbol() {
  if (pending_preview_results_) {
    // Don't rebuild if we're already rebuilding.
    return;
  }

  selected_symbol_name_ = std::nullopt;
  if (selected_symbol_index_ >= 0) {
    selected_symbol_name_ =
        previewable_symbols_[selected_symbol_index_].display_name;
    pending_preview_results_ =
        domain_visualizer_
            ->PrepareSymbolForPreview(
                previewable_symbols_[selected_symbol_index_].module,
                previewable_symbols_[selected_symbol_index_].symbol)
            .watch([this](auto x) {
              self_work_queue_.Enqueue([this, x] {
                pending_preview_results_ = std::nullopt;
                preview_error_ = std::nullopt;

                if (std::holds_alternative<utils::CancelledFuture>(x)) {
                  preview_error_ = utils::Error{"Compilation cancelled."};
                  return;
                }

                const auto& error_or = *std::get<1>(x);
                if (auto error = std::get_if<0>(&error_or)) {
                  preview_error_ = utils::Error{error->msg};
                } else {
                  domain_visualizer_->SetPreview(std::get<1>(error_or));
                }
              });
            });
  }
}

}  // namespace imgui
}  // namespace typescript_cpp_v8
}  // namespace reify
