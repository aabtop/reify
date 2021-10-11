#include "src/visualizer/scene_object_region3.h"

#include <CGAL/boost/graph/convert_nef_polyhedron_to_polygon_mesh.h>

#include <thread>

// clang-format off
#include "imgui.h"
#include "imfilebrowser.h"
// clang-format on

#include "cgal/construct_region3.h"
#include "cgal/errors.h"
#include "cgal/export_to_stl.h"
#include "cgal/types_nef_polyhedron_3.h"
#include "reify/purecpp/hypo.h"

namespace hypo {
namespace visualizer {

namespace {
FlatShadedTriangleRenderer3::TriangleSoup ConvertToTriangleSoup(
    const hypo::cgal::Nef_polyhedron_3& polyhedron) {
  std::vector<hypo::cgal::Point_3> cgal_vertices;
  std::vector<std::vector<size_t>> cgal_faces;
  CGAL::convert_nef_polyhedron_to_polygon_soup(polyhedron, cgal_vertices,
                                               cgal_faces, true);

  std::vector<FlatShadedTriangleRenderer3::TriangleSoup::Triangle> triangles;
  triangles.reserve(cgal_faces.size());
  std::vector<FlatShadedTriangleRenderer3::TriangleSoup::Vertex> vertices;
  vertices.reserve(triangles.size() * 3 *
                   sizeof(FlatShadedTriangleRenderer3::TriangleSoup::Vertex));

  for (const auto& cgal_face : cgal_faces) {
    assert(cgal_face.size() == 3);
    const std::array<hypo::cgal::Point_3, 3> points = {
        cgal_vertices[cgal_face[0]], cgal_vertices[cgal_face[1]],
        cgal_vertices[cgal_face[2]]};
    hypo::cgal::Vector_3 cgal_normal =
        CGAL::normal(points[0], points[1], points[2]);

    FlatShadedTriangleRenderer3::TriangleSoup::Vector3 normal = {
        static_cast<float>(CGAL::to_double(cgal_normal.x())),
        static_cast<float>(CGAL::to_double(cgal_normal.y())),
        static_cast<float>(CGAL::to_double(cgal_normal.z()))};

    for (const auto& point : points) {
      vertices.push_back(FlatShadedTriangleRenderer3::TriangleSoup::Vertex{
          {static_cast<float>(CGAL::to_double(point.x())),
           static_cast<float>(CGAL::to_double(point.y())),
           static_cast<float>(CGAL::to_double(point.z()))},
          normal,
      });
    }

    triangles.push_back(FlatShadedTriangleRenderer3::TriangleSoup::Triangle{
        static_cast<uint32_t>(vertices.size() - 3),
        static_cast<uint32_t>(vertices.size() - 2),
        static_cast<uint32_t>(vertices.size() - 1)});
  }

  return FlatShadedTriangleRenderer3::TriangleSoup{std::move(vertices),
                                                   std::move(triangles)};
}

}  // namespace

reify::utils::ErrorOr<std::shared_ptr<reify::pure_cpp::SceneObject<glm::mat4>>>
CreateSceneObjectRegion3(reify::pure_cpp::ThreadPoolCacheRunner* runner,
                         const hypo::Region3& data) {
  REIFY_UTILS_ASSIGN_OR_RETURN(
      polyhedron3, hypo::cgal::CallCgalAndCatchExceptions(
                       &hypo::cgal::ConstructRegion3, runner, data));
  const std::shared_ptr<const FlatShadedTriangleRenderer3::TriangleSoup>
      triangle_soup(new FlatShadedTriangleRenderer3::TriangleSoup(
          ConvertToTriangleSoup(*polyhedron3)));

  return std::shared_ptr<reify::pure_cpp::SceneObject<glm::mat4>>(
      new SceneObjectRegion3(polyhedron3, triangle_soup));
}

SceneObjectRegion3::SceneObjectRegion3(
    const std::shared_ptr<const hypo::cgal::Nef_polyhedron_3>& polyhedron3,
    const std::shared_ptr<const FlatShadedTriangleRenderer3::TriangleSoup>&
        triangle_soup)
    : polyhedron3_(polyhedron3), triangle_soup_(triangle_soup) {}

SceneObjectRegion3::~SceneObjectRegion3() {}

std::string SceneObjectRegion3::ImGuiWindowPanelTitle() const {
  return "Region3";
}
void SceneObjectRegion3::RenderImGuiWindow() {
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

      hypo::cgal::ExportToSTL(*polyhedron3_,
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
SceneObjectRegion3::CreateSceneObjectRenderable(
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
      new SceneObjectRenderableRegion3(
          std::move(flat_shaded_triangle_renderer)));
}

reify::utils::ErrorOr<reify::window::Window::Renderer::FrameResources>
SceneObjectRenderableRegion3::Render(VkCommandBuffer command_buffer,
                                     const glm::mat4& view_projection_matrix) {
  return flat_shaded_triangle_renderer_->RenderFrame(command_buffer,
                                                     view_projection_matrix);
}

}  // namespace visualizer
}  // namespace hypo
