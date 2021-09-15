#ifndef _HYPO_VISUALIZER_OBJECT_VISUALIZER_REGION3_H
#define _HYPO_VISUALIZER_OBJECT_VISUALIZER_REGION3_H

#include <any>
#include <memory>
#include <optional>
#include <string>

#include "cgal/types_nef_polyhedron_3.h"
#include "reify/pure_cpp/scene_visualizer.h"
#include "reify/pure_cpp/scene_visualizer_camera_3d_arcball.h"
#include "reify/pure_cpp/thread_pool_cache_runner.h"
#include "reify/purecpp/hypo.h"
#include "src/visualizer/vulkan/flat_shaded_triangle_renderer3.h"

namespace ImGui {
// We forward declare this class to avoid including `imfilebrowser.h`, which
// includes <windows.h>, which is huge.
class FileBrowser;
}  // namespace ImGui

namespace hypo {
namespace visualizer {

reify::utils::ErrorOr<std::shared_ptr<reify::pure_cpp::SceneObject<glm::mat4>>>
CreateSceneObjectRegion3(reify::pure_cpp::ThreadPoolCacheRunner* runner,
                         const hypo::Region3& data);

class SceneObjectRegion3 : public reify::pure_cpp::SceneObject<glm::mat4>,
                           public reify::pure_cpp::ImGuiVisualizer {
 public:
  SceneObjectRegion3(
      const std::shared_ptr<const hypo::cgal::Nef_polyhedron_3>& polyhedron3,
      const std::shared_ptr<const FlatShadedTriangleRenderer3::TriangleSoup>&
          triangle_soup);
  ~SceneObjectRegion3();

  reify::utils::ErrorOr<
      std::unique_ptr<reify::pure_cpp::SceneObjectRenderable<glm::mat4>>>
  CreateSceneObjectRenderable(VkInstance instance,
                              VkPhysicalDevice physical_device, VkDevice device,
                              VkFormat output_image_format,
                              VkRenderPass render_pass) override;

  reify::pure_cpp::ImGuiVisualizer* GetImGuiVisualizer() const override {
    return const_cast<SceneObjectRegion3*>(this);
  }

  std::string ImGuiWindowPanelTitle() const override;
  void RenderImGuiWindow() override;

 private:
  const std::shared_ptr<const hypo::cgal::Nef_polyhedron_3> polyhedron3_;
  const std::shared_ptr<const FlatShadedTriangleRenderer3::TriangleSoup>
      triangle_soup_;

  std::unique_ptr<ImGui::FileBrowser> export_file_selector_;
};

class SceneObjectRenderableRegion3
    : public reify::pure_cpp::SceneObjectRenderable<glm::mat4> {
 public:
  SceneObjectRenderableRegion3(std::unique_ptr<FlatShadedTriangleRenderer3>&&
                                   flat_shaded_triangle_renderer)
      : flat_shaded_triangle_renderer_(
            std::move(flat_shaded_triangle_renderer)) {}

  reify::utils::ErrorOr<reify::window::Window::Renderer::FrameResources> Render(
      VkCommandBuffer command_buffer,
      const glm::mat4& view_projection_matrix) override;

 private:
  std::unique_ptr<FlatShadedTriangleRenderer3> flat_shaded_triangle_renderer_;
};

}  // namespace visualizer
}  // namespace hypo

#endif  // _HYPO_VISUALIZER_OBJECT_VISUALIZER_REGION3_H
