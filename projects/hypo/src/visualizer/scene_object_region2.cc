#include "src/visualizer/scene_object_region2.h"

#include <CGAL/Polygon_mesh_processing/polygon_mesh_to_polygon_soup.h>

#include <thread>

// clang-format off
#include "imgui.h"
#include "imfilebrowser.h"
// clang-format on

#include <glm/glm.hpp>

#include "cgal/construct_region2.h"
#include "cgal/embed_2d_in_3d.h"
#include "cgal/export_to_svg.h"
#include "cgal/types_polygons.h"
#include "reify/purecpp/hypo.h"

namespace hypo {

namespace {
TriangleSoup ConvertToTriangleSoup(
    const hypo::cgal::Polygon_set_2& polygon_set) {
  cgal::Constrained_Delaunay_triangulation_2 cdt =
      cgal::TriangulatePolygonSet(polygon_set);

  std::vector<TriangleSoup::Triangle> triangles;
  triangles.reserve(cdt.number_of_faces());
  std::vector<TriangleSoup::Vertex> vertices;
  vertices.reserve(cdt.number_of_vertices());

  for (auto iter = cdt.finite_faces_begin(); iter != cdt.finite_faces_end();
       ++iter) {
    cgal::Constrained_Delaunay_triangulation_2::Face_handle face = iter;
    if (!face->info().in_domain()) continue;

    for (int i = 0; i < 3; ++i) {
      const cgal::Point_2& pt = face->vertex(i)->point();

      vertices.push_back(TriangleSoup::Vertex{
          {static_cast<float>(CGAL::to_double(pt.x())),
           static_cast<float>(CGAL::to_double(pt.y())),
           static_cast<float>(0.0)},
          TriangleSoup::Vector3{0.0, 0.0, -1.0},
      });
    }
    triangles.push_back(
        TriangleSoup::Triangle{static_cast<uint32_t>(vertices.size() - 1),
                               static_cast<uint32_t>(vertices.size() - 2),
                               static_cast<uint32_t>(vertices.size() - 3)});
  }

  return TriangleSoup{std::move(vertices), std::move(triangles)};
}
}  // namespace

reify::utils::ErrorOr<std::shared_ptr<reify::pure_cpp::SceneObject<glm::mat4>>>
CreateSceneObjectRegion2(const hypo::Region2& data) {
  hypo::cgal::Polygon_set_2 polygon_set = hypo::cgal::ConstructRegion2(data);
  const std::shared_ptr<const TriangleSoup> triangle_soup(
      new TriangleSoup(ConvertToTriangleSoup(polygon_set)));

  return std::shared_ptr<reify::pure_cpp::SceneObject<glm::mat4>>(
      new SceneObjectRegion2(std::move(polygon_set), triangle_soup));
}

SceneObjectRegion2::SceneObjectRegion2(
    hypo::cgal::Polygon_set_2&& polygon_set,
    const std::shared_ptr<const TriangleSoup>& triangle_soup)
    : polygon_set_(std::move(polygon_set)), triangle_soup_(triangle_soup) {}

SceneObjectRegion2::~SceneObjectRegion2() {}

std::string SceneObjectRegion2::ImGuiWindowPanelTitle() const {
  return "Region2";
}
void SceneObjectRegion2::RenderImGuiWindow() {
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
}

reify::utils::ErrorOr<
    std::unique_ptr<reify::pure_cpp::SceneObjectRenderable<glm::mat4>>>
SceneObjectRegion2::CreateSceneObjectRenderable(
    VkInstance instance, VkPhysicalDevice physical_device, VkDevice device,
    VkFormat output_image_format) {
  auto renderer_or_error = MeshRenderer::Create(instance, physical_device,
                                                device, output_image_format);
  if (auto error = std::get_if<0>(&renderer_or_error)) {
    return reify::utils::Error{error->msg};
  }

  auto mesh_renderer = std::unique_ptr<MeshRenderer>(
      new MeshRenderer(std::move(std::get<1>(renderer_or_error))));
  mesh_renderer->SetTriangleSoup(triangle_soup_);

  return std::unique_ptr<reify::pure_cpp::SceneObjectRenderable<glm::mat4>>(
      new SceneObjectRenderableRegion2(std::move(mesh_renderer)));
}

reify::utils::ErrorOr<reify::window::Window::Renderer::FrameResources>
SceneObjectRenderableRegion2::Render(VkCommandBuffer command_buffer,
                                     VkFramebuffer framebuffer,
                                     VkImage output_color_image,
                                     const reify::window::Rect& viewport_region,
                                     const glm::mat4& view_projection_matrix) {
  return mesh_renderer_->RenderFrame(
      command_buffer, framebuffer, output_color_image,
      {viewport_region.left, viewport_region.top, viewport_region.right,
       viewport_region.bottom},
      view_projection_matrix);
}

}  // namespace hypo
