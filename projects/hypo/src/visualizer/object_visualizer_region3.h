#ifndef _HYPO_VISUALIZER_OBJECT_VISUALIZER_REGION3_H
#define _HYPO_VISUALIZER_OBJECT_VISUALIZER_REGION3_H

#include <any>
#include <memory>
#include <optional>
#include <string>

#include "cgal/types_nef_polyhedron_3.h"
#include "reify/pure_cpp/object_visualizer.h"
#include "reify/pure_cpp/scene_visualizer_camera_3d_arcball.h"
#include "reify/purecpp/hypo.h"
#include "reify/utils/future.h"
#include "reify/utils/thread_with_work_queue.h"
#include "src/visualizer/vulkan/mesh_renderer.h"
#include "src/visualizer/vulkan/triangle_soup.h"

namespace ImGui {
// We forward declare this class to avoid including `imfilebrowser.h`, which
// includes <windows.h>, which is huge.
class FileBrowser;
}  // namespace ImGui

namespace hypo {

class ObjectVisualizerRegion3
    : public reify::pure_cpp::ObjectVisualizer<hypo::Region3>,
      public reify::window::Window,
      public reify::pure_cpp::ImGuiVisualizer {
 public:
  ObjectVisualizerRegion3();
  ~ObjectVisualizerRegion3();

  reify::utils::Future<reify::utils::ErrorOr<std::any>> PrepareDataForPreview(
      const hypo::Region3& data) override;
  void SetPreview(const std::optional<std::any>& prepared_symbol) override;

  reify::window::Window* GetWindow() const override {
    return const_cast<ObjectVisualizerRegion3*>(this);
  }
  reify::pure_cpp::ImGuiVisualizer* GetImGuiVisualizer() const override {
    return const_cast<ObjectVisualizerRegion3*>(this);
  }

  bool OnInputEvent(const InputEvent& input_event) override;

  void OnViewportResize(const std::array<int, 2>& size) override;

  void AdvanceTime(std::chrono::duration<float> seconds) override;

  reify::utils::ErrorOr<std::unique_ptr<Renderer>> CreateRenderer(
      VkInstance instance, VkPhysicalDevice physical_device, VkDevice device,
      VkFormat output_image_format) override;

  std::string ImGuiWindowPanelTitle() const override;
  void RenderImGuiWindow() override;

 private:
  reify::utils::ThreadWithWorkQueue builder_thread_;

  std::shared_ptr<TriangleSoup> pending_triangle_soup_;
  reify::pure_cpp::SceneVisualizerCamera3dArcball free_camera_viewport_;
  MeshRenderer* mesh_renderer_ = nullptr;

  std::shared_ptr<hypo::cgal::Nef_polyhedron_3> current_preview_;

  std::unique_ptr<ImGui::FileBrowser> export_file_selector_;
};

}  // namespace hypo

#endif  // _HYPO_VISUALIZER_OBJECT_VISUALIZER_REGION3_H
