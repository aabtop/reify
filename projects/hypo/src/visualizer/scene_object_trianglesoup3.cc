#include "src/visualizer/scene_object_trianglesoup3.h"

// clang-format off
#include "imgui.h"
#include "imfilebrowser.h"
// clang-format on

#include "cgal/construct_trianglesoup3.h"
#include "cgal/errors.h"
#include "hypo/geometry/export_to_stl.h"
#include "hypo/geometry/triangle_soup.h"
#include "reify/purecpp/hypo.h"

namespace hypo {
namespace visualizer {

reify::utils::ErrorOr<std::shared_ptr<reify::pure_cpp::SceneObject<glm::mat4>>>
CreateSceneObjectTriangleSoup3(reify::pure_cpp::ThreadPoolCacheRunner* runner,
                               const hypo::TriangleSoup3& data) {
  REIFY_UTILS_ASSIGN_OR_RETURN(
      triangle_soup, hypo::cgal::CallCgalAndCatchExceptions(
                         &hypo::cgal::ConstructTriangleSoup3, runner, data));

  return std::shared_ptr<reify::pure_cpp::SceneObject<glm::mat4>>(
      new SceneObjectTriangleSoup3("TriangleSoup3", triangle_soup));
}

SceneObjectTriangleSoup3::SceneObjectTriangleSoup3(
    const std::string& window_panel_title,
    const std::shared_ptr<const hypo::geometry::TriangleSoup>& triangle_soup)
    : window_panel_title_(window_panel_title), triangle_soup_(triangle_soup) {}

SceneObjectTriangleSoup3::~SceneObjectTriangleSoup3() {}

std::string SceneObjectTriangleSoup3::ImGuiWindowPanelTitle() const {
  return window_panel_title_;
}
void SceneObjectTriangleSoup3::RenderImGuiWindow() {
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

      hypo::geometry::ExportToSTL(*triangle_soup_,
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
SceneObjectTriangleSoup3::CreateSceneObjectRenderable(
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
  flat_shaded_triangle_renderer->SetTriangleSoup(triangle_soup_);

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
