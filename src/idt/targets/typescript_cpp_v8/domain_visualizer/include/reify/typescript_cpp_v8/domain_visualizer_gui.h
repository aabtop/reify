#ifndef _IDT_TARGETS_TYPESCRIPT_CPP_V8_DOMAIN_VISUALIZER_DOMAIN_VISUALIZER_GUI_H
#define _IDT_TARGETS_TYPESCRIPT_CPP_V8_DOMAIN_VISUALIZER_DOMAIN_VISUALIZER_GUI_H

#include "reify/typescript_cpp_v8/domain_visualizer.h"

namespace reify {
namespace typescript_cpp_v8 {

class DomainVisualizerGui : public DomainVisualizer {
 public:
  DomainVisualizerGui(std::unique_ptr<DomainVisualizer> wrapped);

  std::vector<reify::CompilerEnvironment::InputModule> GetTypeScriptModules()
      override;

  bool CanPreviewSymbol(
      const reify::CompiledModule::ExportedSymbol& symbol) override;

  void PrepareSymbolForPreview(
      std::shared_ptr<reify::CompiledModule> module,
      const reify::CompiledModule::ExportedSymbol& symbol,
      const std::function<void(ErrorOr<PreparedSymbol>)>& on_preview_prepared)
      override;
  void Preview(const PreparedSymbol& prepared_symbol) override;

  void OnInputEvent(const InputEvent& input_event) override;

  void OnViewportResize(const std::array<int, 2>& size) override;

  void AdvanceTime(std::chrono::duration<float> seconds) override;

  ErrorOr<std::unique_ptr<Renderer>> CreateRenderer(
      VkInstance instance, VkPhysicalDevice physical_device, VkDevice device,
      VkFormat output_image_format) override;

 private:
  std::unique_ptr<DomainVisualizer> wrapped_;
};

}  // namespace typescript_cpp_v8
}  // namespace reify

#endif  // _IDT_TARGETS_TYPESCRIPT_CPP_V8_DOMAIN_VISUALIZER_DOMAIN_VISUALIZER_GUI_H
