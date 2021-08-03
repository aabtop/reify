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
#include "src/visualizer/vulkan/mesh_renderer.h"
#include "src/visualizer/vulkan/triangle_soup.h"

namespace ImGui {
// We forward declare this class to avoid including `imfilebrowser.h`, which
// includes <windows.h>, which is huge.
class FileBrowser;
}  // namespace ImGui

namespace hypo {

reify::utils::ErrorOr<std::shared_ptr<
    reify::pure_cpp::SceneVisualizer<hypo::Region2, glm::mat4>::SceneObject>>
CreateSceneObjectRegion2(const hypo::Region2& data);

class SceneObjectRegion2
    : public reify::pure_cpp::SceneVisualizer<hypo::Region2,
                                              glm::mat4>::SceneObject,
      public reify::pure_cpp::ImGuiVisualizer {
 public:
  SceneObjectRegion2(hypo::cgal::Polygon_set_2&& polygon_set,
                     const std::shared_ptr<const TriangleSoup>& triangle_soup);
  ~SceneObjectRegion2();

  reify::utils::ErrorOr<std::unique_ptr<reify::pure_cpp::SceneVisualizer<
      hypo::Region2, glm::mat4>::SceneObjectRenderable>>
  CreateSceneObjectRenderable(VkInstance instance,
                              VkPhysicalDevice physical_device, VkDevice device,
                              VkFormat output_image_format) override;

  reify::pure_cpp::ImGuiVisualizer* GetImGuiVisualizer() const override {
    return const_cast<SceneObjectRegion2*>(this);
  }

  std::string ImGuiWindowPanelTitle() const override;
  void RenderImGuiWindow() override;

 private:
  const hypo::cgal::Polygon_set_2 polygon_set_;
  const std::shared_ptr<const TriangleSoup> triangle_soup_;

  std::unique_ptr<ImGui::FileBrowser> export_file_selector_;
};

class SceneObjectRenderableRegion2
    : public reify::pure_cpp::SceneVisualizer<
          hypo::Region2, glm::mat4>::SceneObjectRenderable {
 public:
  SceneObjectRenderableRegion2(std::unique_ptr<MeshRenderer>&& mesh_renderer)
      : mesh_renderer_(std::move(mesh_renderer)) {}

  reify::utils::ErrorOr<reify::window::Window::Renderer::FrameResources> Render(
      VkCommandBuffer command_buffer, VkFramebuffer framebuffer,
      VkImage output_color_image, const reify::window::Rect& viewport_region,
      const glm::mat4& view_projection_matrix) override;

 private:
  std::unique_ptr<MeshRenderer> mesh_renderer_;
};

}  // namespace hypo

#endif  // _HYPO_VISUALIZER_OBJECT_VISUALIZER_REGION2_H
