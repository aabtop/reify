#include "src/visualizer/scene_object_trianglesoup3.h"

// clang-format off
#include "imgui.h"
#include "imfilebrowser.h"
// clang-format on

#include <optional>

#include "cgal/construct_trianglesoup3.h"
#include "cgal/errors.h"
#include "hypo/geometry/export_to_stl.h"
#include "hypo/geometry/triangle_soup.h"
#include "reify/pure_cpp/scene_visualizer_camera_3d_arcball.h"
#include "reify/purecpp/hypo.h"
#include "src/visualizer/vulkan/flat_shaded_triangle_renderer3.h"

namespace hypo {
namespace visualizer {

class SceneObjectTriangleSoupSet3
    : public reify::pure_cpp::SceneObject<glm::mat4>,
      public reify::pure_cpp::ImGuiVisualizer {
 public:
  // We re-use this class for other types, so make the title customizable.
  SceneObjectTriangleSoupSet3(
      const std::string& window_panel_title,
      const std::shared_ptr<const hypo::geometry::TriangleSoupSet>&
          triangle_soup_set);
  ~SceneObjectTriangleSoupSet3();

  reify::utils::ErrorOr<
      std::unique_ptr<reify::pure_cpp::SceneObjectRenderable<glm::mat4>>>
  CreateSceneObjectRenderable(VkInstance instance,
                              VkPhysicalDevice physical_device, VkDevice device,
                              VkFormat output_image_format,
                              VkRenderPass render_pass) override;

  reify::pure_cpp::ImGuiVisualizer* GetImGuiVisualizer() const override {
    return const_cast<SceneObjectTriangleSoupSet3*>(this);
  }

  std::string ImGuiWindowPanelTitle() const override;
  void RenderImGuiWindow() override;

 private:
  const std::string window_panel_title_;
  const std::shared_ptr<const hypo::geometry::TriangleSoupSet>
      triangle_soup_set_;

  std::unique_ptr<ImGui::FileBrowser> export_file_selector_;
};

class SceneObjectRenderableTriangleSoup3
    : public reify::pure_cpp::SceneObjectRenderable<glm::mat4> {
 public:
  SceneObjectRenderableTriangleSoup3(
      std::unique_ptr<FlatShadedTriangleRenderer3>&&
          flat_shaded_triangle_renderer)
      : flat_shaded_triangle_renderer_(
            std::move(flat_shaded_triangle_renderer)) {}

  reify::utils::ErrorOr<reify::window::Window::Renderer::FrameResources> Render(
      VkCommandBuffer command_buffer,
      const glm::mat4& view_projection_matrix) override;

 private:
  std::unique_ptr<FlatShadedTriangleRenderer3> flat_shaded_triangle_renderer_;
};

reify::utils::ErrorOr<std::shared_ptr<reify::pure_cpp::SceneObject<glm::mat4>>>
CreateSceneObjectTriangleSoup3(reify::pure_cpp::ThreadPoolCacheRunner* runner,
                               const hypo::TriangleSoup3& data,
                               const std::string& window_panel_title) {
  return CreateSceneObjectTriangleSoupSet3(
      runner, hypo::TriangleSoupSet3({{reify::New(hypo::TriangleSoup3(data))}}),
      window_panel_title.empty() ? "TriangleSoup3" : window_panel_title);
}

reify::utils::ErrorOr<std::shared_ptr<reify::pure_cpp::SceneObject<glm::mat4>>>
CreateSceneObjectTriangleSoupSet3(
    reify::pure_cpp::ThreadPoolCacheRunner* runner,
    const hypo::TriangleSoupSet3& data, const std::string& window_panel_title) {
  REIFY_UTILS_ASSIGN_OR_RETURN(
      triangle_soup_set,
      hypo::cgal::CallCgalAndCatchExceptions(
          &hypo::cgal::ConstructTriangleSoupSet3, runner, data));

  return std::shared_ptr<reify::pure_cpp::SceneObject<glm::mat4>>(
      new SceneObjectTriangleSoupSet3(
          window_panel_title.empty() ? "TriangleSoupSet3" : window_panel_title,
          triangle_soup_set));
}

SceneObjectTriangleSoupSet3::SceneObjectTriangleSoupSet3(
    const std::string& window_panel_title,
    const std::shared_ptr<const hypo::geometry::TriangleSoupSet>&
        triangle_soup_set)
    : window_panel_title_(window_panel_title),
      triangle_soup_set_(triangle_soup_set) {}

SceneObjectTriangleSoupSet3::~SceneObjectTriangleSoupSet3() {}

std::string SceneObjectTriangleSoupSet3::ImGuiWindowPanelTitle() const {
  return window_panel_title_;
}
void SceneObjectTriangleSoupSet3::RenderImGuiWindow() {
  if (ImGui::Button("Export to STL")) {
    export_file_selector_.reset(
        new ImGui::FileBrowser(ImGuiFileBrowserFlags_CloseOnEsc |
                               ImGuiFileBrowserFlags_EnterNewFilename |
                               ImGuiFileBrowserFlags_CreateNewDir));
    export_file_selector_->SetTitle("Export to STL");
    export_file_selector_->SetTypeFilters({".stl"});
    export_file_selector_->Open();
  }

  if (export_file_selector_) {
    export_file_selector_->Display();
    if (export_file_selector_->HasSelected()) {
      std::filesystem::path selected_path =
          std::filesystem::absolute(export_file_selector_->GetSelected());
      if (!selected_path.has_extension()) {
        selected_path.replace_extension("stl");
      }

      hypo::geometry::ExportToSTL(*triangle_soup_set_,
                                  std::filesystem::absolute(selected_path));
      export_file_selector_->Close();
    }
    if (!export_file_selector_->IsOpened()) {
      export_file_selector_.reset();
    }
  }
}

reify::utils::ErrorOr<
    std::unique_ptr<reify::pure_cpp::SceneObjectRenderable<glm::mat4>>>
SceneObjectTriangleSoupSet3::CreateSceneObjectRenderable(
    VkInstance instance, VkPhysicalDevice physical_device, VkDevice device,
    VkFormat output_image_format, VkRenderPass render_pass) {
  auto renderer_or_error = FlatShadedTriangleRenderer3::Create(
      instance, physical_device, device, output_image_format, render_pass);
  if (auto error = std::get_if<0>(&renderer_or_error)) {
    return reify::utils::Error{error->msg};
  }

  auto flat_shaded_triangle_renderer =
      std::unique_ptr<FlatShadedTriangleRenderer3>(
          new FlatShadedTriangleRenderer3(
              std::move(std::get<1>(renderer_or_error))));
  flat_shaded_triangle_renderer->SetTriangleSoupSet(triangle_soup_set_);

  return std::unique_ptr<reify::pure_cpp::SceneObjectRenderable<glm::mat4>>(
      new SceneObjectRenderableTriangleSoup3(
          std::move(flat_shaded_triangle_renderer)));
}

reify::utils::ErrorOr<reify::window::Window::Renderer::FrameResources>
SceneObjectRenderableTriangleSoup3::Render(
    VkCommandBuffer command_buffer, const glm::mat4& view_projection_matrix) {
  return flat_shaded_triangle_renderer_->RenderFrame(command_buffer,
                                                     view_projection_matrix);
}

}  // namespace visualizer
}  // namespace hypo
