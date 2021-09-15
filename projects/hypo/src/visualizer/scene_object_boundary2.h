#ifndef _HYPO_VISUALIZER_SCENE_OBJECT_BOUNDARY2_H
#define _HYPO_VISUALIZER_SCENE_OBJECT_BOUNDARY2_H

#include <any>
#include <glm/glm.hpp>
#include <memory>
#include <optional>
#include <string>

#include "cgal/types_polygons.h"
#include "reify/pure_cpp/scene_visualizer.h"
#include "reify/pure_cpp/thread_pool_cache_runner.h"
#include "reify/purecpp/hypo.h"
#include "src/visualizer/vulkan/simple_simplex_renderer2.h"

namespace ImGui {
// We forward declare this class to avoid including `imfilebrowser.h`, which
// includes <windows.h>, which is huge.
class FileBrowser;
}  // namespace ImGui

namespace hypo {
namespace visualizer {

reify::utils::ErrorOr<std::shared_ptr<reify::pure_cpp::SceneObject<glm::mat3>>>
CreateSceneObjectBoundary2(reify::pure_cpp::ThreadPoolCacheRunner* runner,
                           const hypo::Boundary2& data);

class SceneObjectBoundary2 : public reify::pure_cpp::SceneObject<glm::mat3>,
                             public reify::pure_cpp::ImGuiVisualizer {
 public:
  using LineSegmentSoup = SimpleSimplexRenderer2::SimplexSoup<2, 1>;
  SceneObjectBoundary2(
      const std::shared_ptr<const hypo::cgal::Polygon_set_2>& polygon_set,
      const std::shared_ptr<const LineSegmentSoup>& line_segment_soup);
  ~SceneObjectBoundary2();

  reify::utils::ErrorOr<
      std::unique_ptr<reify::pure_cpp::SceneObjectRenderable<glm::mat3>>>
  CreateSceneObjectRenderable(VkInstance instance,
                              VkPhysicalDevice physical_device, VkDevice device,
                              VkFormat output_image_format,
                              VkRenderPass render_pass) override;

  reify::pure_cpp::ImGuiVisualizer* GetImGuiVisualizer() const override {
    return const_cast<SceneObjectBoundary2*>(this);
  }

  std::string ImGuiWindowPanelTitle() const override;
  void RenderImGuiWindow() override;

 private:
  const std::shared_ptr<const hypo::cgal::Polygon_set_2> polygon_set_;
  const std::shared_ptr<const LineSegmentSoup> line_segment_soup_;

  std::unique_ptr<ImGui::FileBrowser> export_file_selector_;
};

class SceneObjectRenderableLines2
    : public reify::pure_cpp::SceneObjectRenderable<glm::mat3> {
 public:
  SceneObjectRenderableLines2(
      std::unique_ptr<SimpleSimplexRenderer2>&& line_renderer2)
      : line_renderer2_(std::move(line_renderer2)) {}

  reify::utils::ErrorOr<reify::window::Window::Renderer::FrameResources> Render(
      VkCommandBuffer command_buffer,
      const glm::mat3& view_projection_matrix) override;

 private:
  std::unique_ptr<SimpleSimplexRenderer2> line_renderer2_;
};

}  // namespace visualizer
}  // namespace hypo

#endif  // _HYPO_VISUALIZER_SCENE_OBJECT_BOUNDARY2_H
