#ifndef _IDT_TARGETS_TYPESCRIPT_CPP_V8_SYMBOL_VISUALIZER_H
#define _IDT_TARGETS_TYPESCRIPT_CPP_V8_SYMBOL_VISUALIZER_H

#include <any>
#include <memory>
#include <optional>
#include <string>
#include <vector>

#include "reify/pure_cpp/object_visualizer.h"
#include "reify/typescript_cpp_v8/typescript_cpp_v8.h"
#include "reify/utils/error.h"
#include "reify/utils/future.h"

namespace reify {
namespace typescript_cpp_v8 {

struct TypeScriptSymbolVisualizer {
  using CanPreviewSymbolFunction =
      std::function<bool(const CompiledModule::ExportedSymbol&)>;
  using PrepareSymbolForPreviewFunction =
      std::function<reify::utils::Future<utils::ErrorOr<std::any>>(
          RuntimeEnvironment*, const std::shared_ptr<CompiledModule>&,
          const CompiledModule::ExportedSymbol&)>;
  using SetPreviewFunction =
      std::function<void(const std::optional<std::any>&)>;

  std::string typescript_type;

  reify::window::Window* window;
  pure_cpp::ImGuiVisualizer* im_gui_visualizer;

  CanPreviewSymbolFunction can_preview_symbol;
  PrepareSymbolForPreviewFunction prepare_symbol_for_preview;
  SetPreviewFunction set_preview;
};

// This function is the last point where we have templated type information on
// the type we want to create a visualizer for. It wraps all type-specific
// information into lambdas so that code using the returned object no longer
// needs to know the specific type it's working with.
template <typename T>
std::optional<TypeScriptSymbolVisualizer> MakeTypeScriptSymbolVisualizer(
    pure_cpp::ObjectVisualizer<T>* object_visualizer) {
  return TypeScriptSymbolVisualizer{
      reify_v8::TypeScriptTypeString<T>::value(),
      object_visualizer->GetWindow(),
      object_visualizer->GetImGuiVisualizer(),
      [](const CompiledModule::ExportedSymbol& symbol) {
        return symbol.HasType<reify::typescript_cpp_v8::Function<T()>>();
      },
      [object_visualizer](RuntimeEnvironment* runtime_env,
                          const std::shared_ptr<CompiledModule>& module,
                          const CompiledModule::ExportedSymbol& symbol)
          -> reify::utils::Future<utils::ErrorOr<std::any>> {
        auto entry_point_or_error =
            runtime_env->GetExport<Function<T()>>(symbol.name);
        if (auto error = std::get_if<0>(&entry_point_or_error)) {
          reify::utils::Promise<utils::ErrorOr<std::any>> error_promise;
          error_promise.set(
              utils::Error{"Problem finding entrypoint function: " + *error});
          return error_promise.future();
        }
        auto entry_point = &std::get<1>(entry_point_or_error);

        auto result_or_error = entry_point->Call();
        if (auto error = std::get_if<0>(&result_or_error)) {
          reify::utils::Promise<utils::ErrorOr<std::any>> error_promise;
          error_promise.set(utils::Error{"Error running function: " + *error});
          return error_promise.future();
        }

        return object_visualizer->PrepareDataForPreview(
            std::get<1>(result_or_error));
      },
      [object_visualizer](const std::optional<std::any>& symbol) {
        object_visualizer->SetPreview(symbol);
      }};
}

class SymbolVisualizer : public window::Window {
 public:
  SymbolVisualizer(
      const std::vector<CompilerEnvironment::InputModule>& typescript_modules,
      const std::vector<TypeScriptSymbolVisualizer>& visualizers);

  std::vector<CompilerEnvironment::InputModule> GetTypeScriptModules() {
    return typescript_modules_;
  }

  bool CanPreviewSymbol(const CompiledModule::ExportedSymbol& symbol);

  // Returns the type associated with the visualizer that would get used to
  // visualize the given symbol.
  std::string VisualizerTypeScriptTypeForSymbol(
      const CompiledModule::ExportedSymbol& symbol);

  reify::utils::Future<utils::ErrorOr<std::any>> PrepareSymbolForPreview(
      std::shared_ptr<CompiledModule> module,
      const CompiledModule::ExportedSymbol& symbol);
  void SetPreview(const std::any& prepared_symbol);
  void ClearPreview();

  bool OnInputEvent(const InputEvent& input_event) override;

  void OnViewportResize(const std::array<int, 2>& size) override;

  void AdvanceTime(std::chrono::duration<float> seconds) override;

  utils::ErrorOr<std::unique_ptr<Renderer>> CreateRenderer(
      VkInstance instance, VkPhysicalDevice physical_device, VkDevice device,
      VkFormat output_image_format) override;

  bool HasImGuiWindow() const;
  std::string ImGuiWindowPanelTitle() const;
  void RenderImGuiWindow();

 private:
  class Renderer;

  struct PreparedSymbol {
    std::any processed_data;
    const TypeScriptSymbolVisualizer* associated_visualizer;
    size_t associated_visualizer_index;
  };

  std::optional<size_t> FindVisualizerIndexForSymbol(
      const CompiledModule::ExportedSymbol& symbol) const;

  const std::vector<CompilerEnvironment::InputModule> typescript_modules_;
  const std::vector<TypeScriptSymbolVisualizer> visualizers_;

  reify::utils::ThreadWithWorkQueue runtime_thread_;

  std::optional<PreparedSymbol> selected_symbol_;

  std::optional<std::array<int, 2>> last_viewport_resize_;

  Renderer* renderer_;
};

}  // namespace typescript_cpp_v8
}  // namespace reify

#endif  // _IDT_TARGETS_TYPESCRIPT_CPP_V8_SYMBOL_VISUALIZER_H
