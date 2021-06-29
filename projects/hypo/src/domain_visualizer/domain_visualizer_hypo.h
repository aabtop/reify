#ifndef _IDE_DOMAIN_VISUALIZER_HYPO_H
#define _IDE_DOMAIN_VISUALIZER_HYPO_H

#include "cgal/types_nef_polyhedron_3.h"
#include "reify/typescript_cpp_v8/domain_visualizer.h"
#include "reify/utils/thread_with_work_queue.h"
#include "src/domain_visualizer/free_camera_viewport_3d.h"
#include "src/domain_visualizer/vulkan/mesh_renderer.h"
#include "src/domain_visualizer/vulkan/triangle_soup.h"

namespace ImGui {
// We forward declare this class to avoid including `imfilebrowser.h`, which
// includes <windows.h>, which is huge.
class FileBrowser;
}  // namespace ImGui

class DomainVisualizerHypo : public reify::typescript_cpp_v8::DomainVisualizer {
 public:
  DomainVisualizerHypo();
  ~DomainVisualizerHypo();

  std::vector<reify::typescript_cpp_v8::CompilerEnvironment::InputModule>
  GetTypeScriptModules() override;

  bool CanPreviewSymbol(
      const reify::typescript_cpp_v8::CompiledModule::ExportedSymbol& symbol)
      override;

  reify::utils::Future<ErrorOr<PreparedSymbol>> PrepareSymbolForPreview(
      std::shared_ptr<reify::typescript_cpp_v8::CompiledModule> module,
      const reify::typescript_cpp_v8::CompiledModule::ExportedSymbol& symbol)
      override;
  void SetPreview(const PreparedSymbol& prepared_symbol) override;
  void ClearPreview() override;

  bool OnInputEvent(const InputEvent& input_event) override;

  void OnViewportResize(const std::array<int, 2>& size) override;

  void AdvanceTime(std::chrono::duration<float> seconds) override;

  ErrorOr<std::unique_ptr<Renderer>> CreateRenderer(
      VkInstance instance, VkPhysicalDevice physical_device, VkDevice device,
      VkFormat output_image_format) override;

  bool HasImGuiWindow() const override;
  std::string ImGuiWindowPanelTitle() const override;
  void RenderImGuiWindow() override;

 private:
  reify::utils::ThreadWithWorkQueue builder_thread_;

  std::shared_ptr<TriangleSoup> pending_triangle_soup_;
  FreeCameraViewport3d free_camera_viewport_;
  MeshRenderer* mesh_renderer_ = nullptr;

  std::shared_ptr<hypo::cgal::Nef_polyhedron_3> current_preview_;

  std::unique_ptr<ImGui::FileBrowser> export_file_selector_;
};

#endif  // _IDE_DOMAIN_VISUALIZER_HYPO_H
