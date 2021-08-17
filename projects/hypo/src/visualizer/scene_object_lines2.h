#ifndef _HYPO_VISUALIZER_SCENE_OBJECT_LINES2_H
#define _HYPO_VISUALIZER_SCENE_OBJECT_LINES2_H

#include <any>
#include <glm/glm.hpp>
#include <memory>
#include <optional>
#include <string>

#include "cgal/types_polygons.h"
#include "reify/pure_cpp/scene_visualizer.h"
#include "reify/purecpp/hypo.h"
#include "src/visualizer/vulkan/line_renderer2.h"

namespace ImGui {
// We forward declare this class to avoid including `imfilebrowser.h`, which
// includes <windows.h>, which is huge.
class FileBrowser;
}  // namespace ImGui

namespace hypo {
namespace visualizer {

reify::utils::ErrorOr<std::shared_ptr<reify::pure_cpp::SceneObject<glm::mat3>>>
CreateSceneObjectLines2(const hypo::Region2& data);

class SceneObjectLines2 : public reify::pure_cpp::SceneObject<glm::mat3>,
                          public reify::pure_cpp::ImGuiVisualizer {
 public:
  SceneObjectLines2(hypo::cgal::Polygon_set_2&& polygon_set,
                    const std::shared_ptr<const LineRenderer2::LineSegmentSoup>&
                        line_segment_soup);
  ~SceneObjectLines2();

  reify::utils::ErrorOr<
      std::unique_ptr<reify::pure_cpp::SceneObjectRenderable<glm::mat3>>>
  CreateSceneObjectRenderable(VkInstance instance,
                              VkPhysicalDevice physical_device, VkDevice device,
                              VkFormat output_image_format,
                              VkRenderPass render_pass) override;

  reify::pure_cpp::ImGuiVisualizer* GetImGuiVisualizer() const override {
    return const_cast<SceneObjectLines2*>(this);
  }

  std::string ImGuiWindowPanelTitle() const override;
  void RenderImGuiWindow() override;

 private:
  const hypo::cgal::Polygon_set_2 polygon_set_;
  const std::shared_ptr<const LineRenderer2::LineSegmentSoup>
      line_segment_soup_;

  std::unique_ptr<ImGui::FileBrowser> export_file_selector_;
};

class SceneObjectRenderableLines2
    : public reify::pure_cpp::SceneObjectRenderable<glm::mat3> {
 public:
  SceneObjectRenderableLines2(std::unique_ptr<LineRenderer2>&& line_renderer2)
      : line_renderer2_(std::move(line_renderer2)) {}

  reify::utils::ErrorOr<reify::window::Window::Renderer::FrameResources> Render(
      VkCommandBuffer command_buffer,
      const glm::mat3& view_projection_matrix) override;

 private:
  std::unique_ptr<LineRenderer2> line_renderer2_;
};

}  // namespace visualizer
}  // namespace hypo

#endif  // _HYPO_VISUALIZER_SCENE_OBJECT_LINES2_H
