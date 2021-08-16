#ifndef _HYPO_VISUALIZER_OBJECT_VISUALIZER_REGION2_H
#define _HYPO_VISUALIZER_OBJECT_VISUALIZER_REGION2_H

#include <any>
#include <glm/glm.hpp>
#include <memory>
#include <optional>
#include <string>

#include "cgal/types_polygons.h"
#include "reify/pure_cpp/scene_visualizer.h"
#include "reify/purecpp/hypo.h"
#include "src/visualizer/vulkan/flat_triangle_renderer2.h"

namespace ImGui {
// We forward declare this class to avoid including `imfilebrowser.h`, which
// includes <windows.h>, which is huge.
class FileBrowser;
}  // namespace ImGui

namespace hypo {
namespace visualizer {

reify::utils::ErrorOr<std::shared_ptr<reify::pure_cpp::SceneObject<glm::mat3>>>
CreateSceneObjectRegion2(const hypo::Region2& data);

class SceneObjectRegion2 : public reify::pure_cpp::SceneObject<glm::mat3>,
                           public reify::pure_cpp::ImGuiVisualizer {
 public:
  SceneObjectRegion2(
      hypo::cgal::Polygon_set_2&& polygon_set,
      const std::shared_ptr<const FlatTriangleRenderer2::TriangleSoup>&
          triangle_soup,
      const std::shared_ptr<reify::pure_cpp::SceneObject<glm::mat3>>&
          scene_object_lines);
  ~SceneObjectRegion2();

  reify::utils::ErrorOr<
      std::unique_ptr<reify::pure_cpp::SceneObjectRenderable<glm::mat3>>>
  CreateSceneObjectRenderable(VkInstance instance,
                              VkPhysicalDevice physical_device, VkDevice device,
                              VkFormat output_image_format,
                              VkRenderPass render_pass) override;

  reify::pure_cpp::ImGuiVisualizer* GetImGuiVisualizer() const override {
    return const_cast<SceneObjectRegion2*>(this);
  }

  std::string ImGuiWindowPanelTitle() const override;
  void RenderImGuiWindow() override;

 private:
  friend class SceneObjectRenderableRegion2;

  const hypo::cgal::Polygon_set_2 polygon_set_;
  const std::shared_ptr<const FlatTriangleRenderer2::TriangleSoup>
      triangle_soup_;

  std::unique_ptr<ImGui::FileBrowser> export_file_selector_;

  // Used whenever the user chooses to view the outline.
  std::shared_ptr<reify::pure_cpp::SceneObject<glm::mat3>> scene_object_lines_;

  bool fill_region_interior_ = true;
  bool show_region_outline_ = false;
};

class SceneObjectRenderableRegion2
    : public reify::pure_cpp::SceneObjectRenderable<glm::mat3> {
 public:
  SceneObjectRenderableRegion2(
      SceneObjectRegion2* parent,
      std::unique_ptr<FlatTriangleRenderer2>&& flat_triangle_renderer2,
      std::unique_ptr<reify::pure_cpp::SceneObjectRenderable<glm::mat3>>&&
          lines_renderer)
      : parent_(parent),
        flat_triangle_renderer2_(std::move(flat_triangle_renderer2)),
        lines_renderer_(std::move(lines_renderer)) {}

  reify::utils::ErrorOr<reify::window::Window::Renderer::FrameResources> Render(
      VkCommandBuffer command_buffer,
      const glm::mat3& view_projection_matrix) override;

 private:
  SceneObjectRegion2* parent_;
  std::unique_ptr<FlatTriangleRenderer2> flat_triangle_renderer2_;

  // For drawing the outlines, if the user chooses to enable outlines.
  std::unique_ptr<reify::pure_cpp::SceneObjectRenderable<glm::mat3>>
      lines_renderer_;
};

}  // namespace visualizer
}  // namespace hypo

#endif  // _HYPO_VISUALIZER_OBJECT_VISUALIZER_REGION2_H
