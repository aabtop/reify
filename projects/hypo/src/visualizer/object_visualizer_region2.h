#ifndef _HYPO_VISUALIZER_OBJECT_VISUALIZER_REGION2_H
#define _HYPO_VISUALIZER_OBJECT_VISUALIZER_REGION2_H

#include <any>
#include <glm/glm.hpp>
#include <memory>
#include <optional>
#include <string>

#include "cgal/types_polygons.h"
#include "reify/pure_cpp/object_visualizer.h"
#include "reify/purecpp/hypo.h"
#include "reify/utils/future.h"
#include "reify/utils/thread_with_work_queue.h"
#include "reify/window/window.h"
#include "src/visualizer/vulkan/mesh_renderer.h"
#include "src/visualizer/vulkan/triangle_soup.h"
#include "src/visualizer/camera_2d.h"

namespace ImGui {
// We forward declare this class to avoid including `imfilebrowser.h`, which
// includes <windows.h>, which is huge.
class FileBrowser;
}  // namespace ImGui

namespace hypo {

class ObjectVisualizerRegion2
    : public reify::pure_cpp::ObjectVisualizer<hypo::Region2>,
      public reify::window::Window,
      public reify::pure_cpp::ImGuiVisualizer {
 public:
  ObjectVisualizerRegion2();
  ~ObjectVisualizerRegion2();

  reify::utils::Future<reify::utils::ErrorOr<std::any>> PrepareDataForPreview(
      const hypo::Region2& data) override;
  void SetPreview(const std::optional<std::any>& prepared_symbol) override;

  reify::window::Window* GetWindow() const override {
    return const_cast<ObjectVisualizerRegion2*>(this);
  }
  reify::pure_cpp::ImGuiVisualizer* GetImGuiVisualizer() const override {
    return const_cast<ObjectVisualizerRegion2*>(this);
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
  glm::vec2 ToViewportPoint(int x, int y) const;

  reify::utils::ThreadWithWorkQueue builder_thread_;

  std::shared_ptr<TriangleSoup> pending_triangle_soup_;
  Camera2d camera_;
  bool mouse_button_pressed_[static_cast<int>(MouseButton::Count)];
  MeshRenderer* mesh_renderer_ = nullptr;

  std::shared_ptr<hypo::cgal::Polygon_set_2> current_preview_;

  std::unique_ptr<ImGui::FileBrowser> export_file_selector_;
};

}  // namespace hypo

#endif  // _HYPO_VISUALIZER_OBJECT_VISUALIZER_REGION2_H
