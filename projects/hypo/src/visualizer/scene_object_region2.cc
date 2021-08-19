#include "src/visualizer/scene_object_region2.h"

#include <CGAL/Polygon_mesh_processing/polygon_mesh_to_polygon_soup.h>

#include <thread>

// clang-format off
#include "imgui.h"
#include "imfilebrowser.h"
// clang-format on

#include <fmt/format.h>

#include <glm/glm.hpp>

#include "cgal/construct_region2.h"
#include "cgal/embed_2d_in_3d.h"
#include "cgal/export_to_svg.h"
#include "cgal/types_polygons.h"
#include "reify/purecpp/hypo.h"
#include "src/visualizer/scene_object_boundary2.h"

namespace hypo {
namespace visualizer {

namespace {
SceneObjectRegion2::TriangleSoup ConvertToTriangleSoup(
    const hypo::cgal::Polygon_set_2& polygon_set) {
  cgal::Constrained_Delaunay_triangulation_2 cdt =
      cgal::TriangulatePolygonSet(polygon_set);

  std::vector<SceneObjectRegion2::TriangleSoup::Simplex> triangles;
  triangles.reserve(cdt.number_of_faces());
  std::vector<SceneObjectRegion2::TriangleSoup::Vertex> vertices;
  vertices.reserve(cdt.number_of_vertices());

  for (auto iter = cdt.finite_faces_begin(); iter != cdt.finite_faces_end();
       ++iter) {
    cgal::Constrained_Delaunay_triangulation_2::Face_handle face = iter;
    if (!face->info().in_domain()) continue;

    for (int i = 0; i < 3; ++i) {
      const cgal::Point_2& pt = face->vertex(i)->point();

      vertices.push_back(SceneObjectRegion2::TriangleSoup::Vertex{
          {static_cast<float>(CGAL::to_double(pt.x())),
           static_cast<float>(CGAL::to_double(pt.y()))}});
    }
    triangles.push_back(SceneObjectRegion2::TriangleSoup::Simplex{
        static_cast<uint32_t>(vertices.size() - 1),
        static_cast<uint32_t>(vertices.size() - 2),
        static_cast<uint32_t>(vertices.size() - 3)});
  }

  return SceneObjectRegion2::TriangleSoup{std::move(vertices),
                                          std::move(triangles)};
}
}  // namespace

reify::utils::ErrorOr<std::shared_ptr<reify::pure_cpp::SceneObject<glm::mat3>>>
CreateSceneObjectRegion2(const hypo::Region2& data) {
  hypo::cgal::Polygon_set_2 polygon_set = hypo::cgal::ConstructRegion2(data);
  const std::shared_ptr<const SceneObjectRegion2::TriangleSoup> triangle_soup(
      new SceneObjectRegion2::TriangleSoup(ConvertToTriangleSoup(polygon_set)));

  REIFY_UTILS_ASSIGN_OR_RETURN(
      scene_object_boundary2,
      CreateSceneObjectBoundary2(hypo::Boundary2{data}));

  return std::shared_ptr<reify::pure_cpp::SceneObject<glm::mat3>>(
      new SceneObjectRegion2(std::move(polygon_set), triangle_soup,
                             std::move(scene_object_boundary2)));
}

SceneObjectRegion2::SceneObjectRegion2(
    hypo::cgal::Polygon_set_2&& polygon_set,
    const std::shared_ptr<const TriangleSoup>& triangle_soup,
    const std::shared_ptr<reify::pure_cpp::SceneObject<glm::mat3>>&
        scene_object_boundary2)
    : polygon_set_(std::move(polygon_set)),
      triangle_soup_(triangle_soup),
      scene_object_boundary2_(scene_object_boundary2) {}

SceneObjectRegion2::~SceneObjectRegion2() {}

std::string SceneObjectRegion2::ImGuiWindowPanelTitle() const {
  return "Region2";
}
void SceneObjectRegion2::RenderImGuiWindow() {
  ImGui::Checkbox("Fill region interior", &fill_region_interior_);

  if (ImGui::Button("Export to SVG")) {
    export_file_selector_.reset(
        new ImGui::FileBrowser(ImGuiFileBrowserFlags_CloseOnEsc |
                               ImGuiFileBrowserFlags_EnterNewFilename |
                               ImGuiFileBrowserFlags_CreateNewDir));
    export_file_selector_->SetTitle("Export to SVG");
    export_file_selector_->SetTypeFilters({".svg"});
    export_file_selector_->Open();
  }

  if (export_file_selector_) {
    export_file_selector_->Display();
    if (export_file_selector_->HasSelected()) {
      std::filesystem::path selected_path =
          std::filesystem::absolute(export_file_selector_->GetSelected());
      if (!selected_path.has_extension()) {
        selected_path.replace_extension("svg");
      }

      hypo::cgal::ExportToSVG(polygon_set_,
                              std::filesystem::absolute(selected_path));
      export_file_selector_->Close();
    }
    if (!export_file_selector_->IsOpened()) {
      export_file_selector_.reset();
    }
  }

  ImGui::Checkbox("Show region outline", &show_region_outline_);
  if (show_region_outline_) {
    auto boundary2_imgui = scene_object_boundary2_->GetImGuiVisualizer();
    if (boundary2_imgui) {
      if (ImGui::CollapsingHeader(
              fmt::format("Outline options ({})",
                          boundary2_imgui->ImGuiWindowPanelTitle())
                  .c_str())) {
        boundary2_imgui->RenderImGuiWindow();
      }
    }
  }
}

reify::utils::ErrorOr<
    std::unique_ptr<reify::pure_cpp::SceneObjectRenderable<glm::mat3>>>
SceneObjectRegion2::CreateSceneObjectRenderable(
    VkInstance instance, VkPhysicalDevice physical_device, VkDevice device,
    VkFormat output_image_format, VkRenderPass render_pass) {
  auto renderer_or_error = SimpleSimplexRenderer2::Create(
      instance, physical_device, device, output_image_format, render_pass, 2);
  if (auto error = std::get_if<0>(&renderer_or_error)) {
    return reify::utils::Error{error->msg};
  }

  auto flag_triangle_renderer = std::unique_ptr<SimpleSimplexRenderer2>(
      new SimpleSimplexRenderer2(std::move(std::get<1>(renderer_or_error))));
  if (triangle_soup_) {
    flag_triangle_renderer->SetSimplexSoup(*triangle_soup_);
  } else {
    flag_triangle_renderer->ClearSimplexSoup();
  }

  REIFY_UTILS_ASSIGN_OR_RETURN(
      boundary2_renderer,
      scene_object_boundary2_->CreateSceneObjectRenderable(
          instance, physical_device, device, output_image_format, render_pass));

  return std::unique_ptr<reify::pure_cpp::SceneObjectRenderable<glm::mat3>>(
      new SceneObjectRenderableRegion2(this, std::move(flag_triangle_renderer),
                                       std::move(boundary2_renderer)));
}

reify::utils::ErrorOr<reify::window::Window::Renderer::FrameResources>
SceneObjectRenderableRegion2::Render(VkCommandBuffer command_buffer,
                                     const glm::mat3& view_projection_matrix) {
  std::vector<reify::window::Window::Renderer::FrameResources> frame_resources;

  if (parent_->fill_region_interior_) {
    REIFY_UTILS_ASSIGN_OR_RETURN(fill_region_frame_resources,
                                 flat_triangle_renderer2_->RenderFrame(
                                     command_buffer, view_projection_matrix,
                                     glm::vec4(0.8, 0.8, 0.8, 1.0)));

    frame_resources.push_back(fill_region_frame_resources);
  }

  if (parent_->show_region_outline_) {
    REIFY_UTILS_ASSIGN_OR_RETURN(
        boundary2_renderer_frame_resources,
        boundary2_renderer_->Render(command_buffer, view_projection_matrix));
    frame_resources.push_back(boundary2_renderer_frame_resources);
  }

  return frame_resources;
}

}  // namespace visualizer
}  // namespace hypo
