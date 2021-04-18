#ifndef _IDE_DOMAIN_VISUALIZER_HYPO_H
#define _IDE_DOMAIN_VISUALIZER_HYPO_H

#include "reify/typescript_cpp_v8/domain_visualizer.h"
#include "src/ide/free_camera_viewport_3d.h"
#include "src/ide/vulkan/mesh_renderer.h"
#include "src/ide/vulkan/triangle_soup.h"

class DomainVisualizerHypo : public reify::typescript_cpp_v8::DomainVisualizer {
 public:
  DomainVisualizerHypo();

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
  std::shared_ptr<TriangleSoup> pending_triangle_soup_;
  FreeCameraViewport3d free_camera_viewport_;
  MeshRenderer* mesh_renderer_;
};

#endif  // _IDE_DOMAIN_VISUALIZER_HYPO_H
