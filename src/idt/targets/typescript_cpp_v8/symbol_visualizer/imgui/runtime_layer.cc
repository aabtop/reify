#include "reify/typescript_cpp_v8/imgui/runtime_layer.h"

#include <fmt/format.h>
#include <inttypes.h>

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
    SymbolVisualizer* symbol_visualizer,
    const pure_cpp::ThreadPoolCacheRunner& thread_pool_cache_runner)
    : docking_layer_(docking_layer),
      status_layer_(status_layer),
      symbol_visualizer_(symbol_visualizer),
      self_work_queue_(enqueue_task_function),
      thread_pool_cache_runner_(thread_pool_cache_runner) {}

RuntimeLayer::PreviewableSymbols
RuntimeLayer::ComputePreviewableSymbolsFromCompileResults(
    const CompilerEnvironmentThreadSafe::MultiCompileResults& compile_results,
    const std::function<bool(CompiledModule::ExportedSymbol)>& can_preview) {
  PreviewableSymbols previewable_symbols;
  for (const auto& compile_result : compile_results) {
    if (std::holds_alternative<CompileError>(compile_result.second)) {
      // For now we just silently skip over errors.
      continue;
    }

    std::shared_ptr<CompiledModule> compiled_module =
        std::get<1>(compile_result.second);

    PreviewableSymbols::iterator module_entry = previewable_symbols.end();
    for (const auto& symbol : compiled_module->exported_symbols()) {
      if (can_preview(symbol)) {
        if (module_entry == previewable_symbols.end()) {
          module_entry =
              previewable_symbols
                  .insert(std::make_pair(
                      compile_result.first,
                      PreviewableModuleEntry{compile_result.first,
                                             std::get<1>(compile_result.second),
                                             {}}))
                  .first;
        }
        module_entry->second.symbols.push_back(symbol);
      }
    }
  }
  return previewable_symbols;
}

void RuntimeLayer::SetCompileResults(
    const CompilerEnvironmentThreadSafe::MultiCompileResults& compile_results) {
  preview_error_ = std::nullopt;
  compile_results_ = compile_results;
  previewable_symbols_ = ComputePreviewableSymbolsFromCompileResults(
      compile_results_,
      [this](auto x) { return symbol_visualizer_->CanPreviewSymbol(x); });

  if (previewable_symbols_.empty()) {
    selected_symbol_ = std::nullopt;
  } else if (selected_symbol_) {
    auto found = previewable_symbols_.find(
        selected_symbol_->previewable_entry.module_path);
    if (found == previewable_symbols_.end()) {
      // The selected module path is no longer exists or no longer has any
      // previewable symbols.
      selected_symbol_ = std::nullopt;
    } else {
      const std::string& selected_symbol_name =
          selected_symbol_->previewable_entry
              .symbols[selected_symbol_->symbol_preview_index]
              .name;
      const auto& new_symbols = found->second.symbols;
      size_t new_selected_symbol_index = 0;
      for (; new_selected_symbol_index < new_symbols.size();
           ++new_selected_symbol_index) {
        if (new_symbols[new_selected_symbol_index].name ==
            selected_symbol_name) {
          break;
        }
      }
      if (new_selected_symbol_index != new_symbols.size()) {
        selected_symbol_ =
            SelectedSymbol{found->second, new_selected_symbol_index};
      } else {
        // The symbol originally selected no longer exists :(.
        selected_symbol_ = std::nullopt;
      }
    }
  }

  if (!selected_symbol_) {
    preview_active = false;
    symbol_visualizer_->ClearPreview();
  }

  // If there's only one symbol, and we don't have anything selected, then
  // by default select the symbol.
  if (previewable_symbols_.size() == 1 &&
      previewable_symbols_.begin()->second.symbols.size() == 1) {
    selected_symbol_ = SelectedSymbol{previewable_symbols_.begin()->second, 0};
  }
  RebuildSelectedSymbol();
}

namespace {
bool CompileResultsContainErrors(
    const CompilerEnvironmentThreadSafe::MultiCompileResults& compile_results) {
  for (const auto& item : compile_results) {
    if (std::holds_alternative<CompileError>(item.second)) {
      return true;
    }
  }
  return false;
}

}  // namespace

RuntimeLayer::SymbolTreeNode RuntimeLayer::VirtualPathSetToComponentTrie(
    const std::set<VirtualFilesystem::AbsolutePath>& paths) {
  SymbolTreeNode root;
  for (const auto& path : paths) {
    SymbolTreeNode* current = &root;
    for (const auto& component : path.components()) {
      auto found = current->find(component);
      if (found == current->end()) {
        found =
            current->insert(found, std::make_pair(component, SymbolTreeNode{}));
      }
      current = &(found->second);
    }
  }
  return root;
}

void RuntimeLayer::ExecuteImGuiCommands() {
  {
    const char* SYSTEM_WINDOW_NAME = "System";

    if (!ImGui::FindWindowByName(SYSTEM_WINDOW_NAME)) {
      // If this is the first time we're seeing this window, default it into
      // the docked content menu.
      ImGui::SetNextWindowDockID(docking_layer_->GetDockedContentNodeId());
    }

    ImGui::Begin(SYSTEM_WINDOW_NAME);
    if (ImGui::TreeNodeEx("Cache", ImGuiTreeNodeFlags_DefaultOpen)) {
      ImGui::Text("Total System Memory: %" PRId64,
                  thread_pool_cache_runner_.max_cache_capacity());
      ImGui::TreePop();
    }
    ImGui::End();
  }

  // Place this window within the right dock ID by default.
  const char* SELECT_SYMBOL_WINDOW_NAME = "Select Symbol";
  if (!ImGui::FindWindowByName(SELECT_SYMBOL_WINDOW_NAME)) {
    // If this is the first time we're seeing this window, default it into
    // the docked content menu.
    ImGui::SetNextWindowDockID(docking_layer_->GetDockedContentNodeId());
  }

  if (visualizer_window_newly_opened_) {
    visualizer_window_newly_opened_ = false;
    ImGui::SetNextWindowFocus();
  }
  ImGui::PushStyleVar(ImGuiStyleVar_WindowMinSize, ImVec2(400, 250));
  ImGui::Begin(SELECT_SYMBOL_WINDOW_NAME);

  if (!previewable_symbols_.empty()) {
    DisableIf disable_if_pending_preview_results(!!pending_preview_results_);
    ImGui::PushItemWidth(-1);

    std::set<VirtualFilesystem::AbsolutePath> module_path_set;
    for (const auto& previewable_symbol : previewable_symbols_) {
      module_path_set.insert(previewable_symbol.second.module_path);
    }
    RenderSymbolTree({}, VirtualPathSetToComponentTrie(module_path_set));
    ImGui::PopItemWidth();
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
        if (ImGui::CollapsingHeader(item.first.string().c_str(),
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
            "Building %s:%s...",
            selected_symbol_->previewable_entry.module_path.string().c_str(),
            selected_symbol_->previewable_entry
                .symbols[selected_symbol_->symbol_preview_index]
                .name.c_str());
      });
    }

  } else {
    status_window_ = std::nullopt;
  }

  if (preview_active && symbol_visualizer_->HasImGuiWindow()) {
    std::string visualizer_window_title =
        symbol_visualizer_->ImGuiWindowPanelTitle();

    if (!ImGui::FindWindowByName(visualizer_window_title.c_str())) {
      // If this is the first time we're seeing this window, default it into
      // the bottom dock slot.
      ImGui::SetNextWindowDockID(docking_layer_->GetDockedContentNodeId());
    }
    ImGui::Begin(visualizer_window_title.c_str());
    symbol_visualizer_->RenderImGuiWindow();

    if (ImGui::IsWindowAppearing()) {
      visualizer_window_newly_opened_ = true;
    }
    ImGui::End();
  }
}

void RuntimeLayer::RenderSymbolTree(
    const std::vector<std::string>& previous_components,
    const SymbolTreeNode& tree) {
  if (tree.empty()) {
    // Base case
    auto path =
        *VirtualFilesystem::AbsolutePath::FromComponents(previous_components);
    const auto& previewable_symbol_entry =
        previewable_symbols_.find(path)->second;
    const auto& symbols = previewable_symbol_entry.symbols;
    bool is_selected_module =
        selected_symbol_ &&
        selected_symbol_->previewable_entry.module_path == path;

    for (size_t i = 0; i < symbols.size(); ++i) {
      const auto& symbol = symbols[i];
      ImGuiTreeNodeFlags flags = ImGuiTreeNodeFlags_Leaf |
                                 ImGuiTreeNodeFlags_NoTreePushOnOpen |
                                 ImGuiTreeNodeFlags_SpanFullWidth;
      if (is_selected_module && i == selected_symbol_->symbol_preview_index) {
        flags |= ImGuiTreeNodeFlags_Selected;
      }
      std::string visualizer_type =
          symbol_visualizer_->VisualizerTypeScriptTypeForSymbol(symbol);
      ImGui::TreeNodeEx(symbol.name.c_str(), flags, "%s (%s)",
                        symbol.name.c_str(), visualizer_type.c_str());
      if (ImGui::IsItemClicked()) {
        selected_symbol_ = SelectedSymbol{previewable_symbol_entry, i};
        RebuildSelectedSymbol();
      }
    }
  } else {
    // Recursive case
    for (const auto& child : tree) {
      std::vector<std::string> current_components;

      // Group together strings of single children.
      const SymbolTreeNode::value_type* current_child = &child;
      while (true) {
        current_components.push_back(current_child->first);
        if (current_child->second.size() != 1) {
          break;
        }
        current_child = &(*current_child->second.begin());
      }

      std::vector<std::string> next_components;
      next_components.reserve(previous_components.size() +
                              current_components.size());
      next_components.insert(next_components.end(), previous_components.begin(),
                             previous_components.end());
      next_components.insert(next_components.end(), current_components.begin(),
                             current_components.end());

      ImGuiTreeNodeFlags flags = ImGuiTreeNodeFlags_SpanFullWidth;
      if (previewable_symbols_.size() == 1) {
        // If there's only one module, show it automatically expanded.
        flags = ImGuiTreeNodeFlags_DefaultOpen;
      }
      if (ImGui::TreeNodeEx(VirtualFilesystem::RelativePath(current_components)
                                .string()
                                .c_str(),
                            flags)) {
        RenderSymbolTree(next_components, current_child->second);
        ImGui::TreePop();
      }
    }
  }
}

void RuntimeLayer::RebuildSelectedSymbol() {
  if (pending_preview_results_) {
    // Don't rebuild if we're already rebuilding.
    return;
  }

  if (!selected_symbol_) {
    // Nothing is selected, so there's nothing to rebuild.
    return;
  }

  pending_preview_results_ =
      symbol_visualizer_
          ->PrepareSymbolForPreview(
              selected_symbol_->previewable_entry.module,
              selected_symbol_->previewable_entry
                  .symbols[selected_symbol_->symbol_preview_index])
          .watch([this](auto x) {
            self_work_queue_.Enqueue([this, x] {
              pending_preview_results_ = std::nullopt;
              preview_error_ = std::nullopt;

              auto make_error = [this](const std::string& msg) {
                return utils::Error{fmt::format(
                    "{}:{}: {}",
                    selected_symbol_->previewable_entry.module_path.string(),
                    selected_symbol_->previewable_entry
                        .symbols[selected_symbol_->symbol_preview_index]
                        .name,
                    msg)};
              };

              if (std::holds_alternative<utils::CancelledFuture>(x)) {
                preview_error_ = make_error("Compilation cancelled.");
                return;
              }

              const auto& error_or = *std::get<1>(x);
              if (auto error = std::get_if<0>(&error_or)) {
                preview_error_ = make_error(error->msg);
              } else {
                symbol_visualizer_->SetPreview(std::get<1>(error_or));
                preview_active = true;
              }
            });
          });
}

}  // namespace imgui
}  // namespace typescript_cpp_v8
}  // namespace reify
