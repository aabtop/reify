#ifndef _HYPO_VISUALIZER_OBJECT_VISUALIZER_SVG_ELEMENTS_H
#define _HYPO_VISUALIZER_OBJECT_VISUALIZER_SVG_ELEMENTS_H

#include "reify/pure_cpp/scene_visualizer.h"
#include "reify/pure_cpp/thread_pool_cache_runner.h"
#include "reify/purecpp/hypo.h"
#include "reify/utils/error.h"
#include "svg/svg_types.h"
#include "visualizer/visualizer_svg_types.h"

namespace ImGui {
// We forward declare this class to avoid including `imfilebrowser.h`, which
// includes <windows.h>, which is huge.
class FileBrowser;
}  // namespace ImGui

namespace hypo {
namespace visualizer {

reify::utils::ErrorOr<std::shared_ptr<reify::pure_cpp::SceneObject<glm::mat3>>>
CreateSceneObjectSvgElements(
    reify::pure_cpp::ThreadPoolCacheRunner* runner,
    const hypo::SvgElements& data,
    const std::string& window_panel_title = std::string());

reify::utils::ErrorOr<std::shared_ptr<reify::pure_cpp::SceneObject<glm::mat3>>>
CreateSceneObjectSvgElement(
    reify::pure_cpp::ThreadPoolCacheRunner* runner,
    const hypo::SvgElement& data,
    const std::string& window_panel_title = std::string());

class SceneObjectSvgElements : public reify::pure_cpp::SceneObject<glm::mat3>,
                               public reify::pure_cpp::ImGuiVisualizer {
 public:
  SceneObjectSvgElements(
      const std::shared_ptr<const VisualizerSvgElements>&
          visualizer_svg_elements,
      const std::shared_ptr<const svg::Elements>& svg_elements,
      const std::string& window_panel_title);
  ~SceneObjectSvgElements();

  reify::utils::ErrorOr<
      std::unique_ptr<reify::pure_cpp::SceneObjectRenderable<glm::mat3>>>
  CreateSceneObjectRenderable(VkInstance instance,
                              VkPhysicalDevice physical_device, VkDevice device,
                              VkFormat output_image_format,
                              VkRenderPass render_pass) override;

  reify::pure_cpp::ImGuiVisualizer* GetImGuiVisualizer() const override {
    return const_cast<SceneObjectSvgElements*>(this);
  }

  std::string ImGuiWindowPanelTitle() const override;
  void RenderImGuiWindow() override;

 private:
  friend class SceneObjectRenderableSvgElements;

  const std::shared_ptr<const svg::Elements>
      svg_elements_;  // Used for exporting.
  const std::shared_ptr<const VisualizerSvgElements>
      visualizer_svg_elements_;  // Used for rendering.

  const std::string window_panel_title_;

  std::unique_ptr<ImGui::FileBrowser> export_file_selector_;
};

class SceneObjectRenderableSvgElements
    : public reify::pure_cpp::SceneObjectRenderable<glm::mat3> {
 public:
  SceneObjectRenderableSvgElements(
      SceneObjectSvgElements* parent,
      std::vector<SimpleSimplexRenderer2>&& element_renderers)
      : parent_(parent), element_renderers_(std::move(element_renderers)) {}

  reify::utils::ErrorOr<reify::window::Window::Renderer::FrameResources> Render(
      VkCommandBuffer command_buffer,
      const glm::mat3& view_projection_matrix) override;

 private:
  SceneObjectSvgElements* parent_;
  std::vector<SimpleSimplexRenderer2> element_renderers_;
};

}  // namespace visualizer
}  // namespace hypo

#endif  // _HYPO_VISUALIZER_OBJECT_VISUALIZER_SVG_ELEMENTS_H
