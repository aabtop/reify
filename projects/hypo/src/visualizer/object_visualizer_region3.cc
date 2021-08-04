#include "src/visualizer/object_visualizer_region3.h"

#include <CGAL/boost/graph/convert_nef_polyhedron_to_polygon_mesh.h>

#include <thread>

// clang-format off
#include "imgui.h"
#include "imfilebrowser.h"
// clang-format on

#include "cgal/construct_region3.h"
#include "cgal/export_to_stl.h"
#include "cgal/types_nef_polyhedron_3.h"
#include "reify/purecpp/hypo.h"

namespace hypo {

namespace {
TriangleSoup ConvertToTriangleSoup(
    const hypo::cgal::Nef_polyhedron_3& polyhedron) {
  std::vector<hypo::cgal::Point_3> cgal_vertices;
  std::vector<std::vector<size_t>> cgal_faces;
  CGAL::convert_nef_polyhedron_to_polygon_soup(polyhedron, cgal_vertices,
                                               cgal_faces, true);

  std::vector<TriangleSoup::Triangle> triangles;
  triangles.reserve(cgal_faces.size());
  std::vector<TriangleSoup::Vertex> vertices;
  vertices.reserve(triangles.size() * 3 * sizeof(TriangleSoup::Vertex));

  for (const auto& cgal_face : cgal_faces) {
    assert(cgal_face.size() == 3);
    const std::array<hypo::cgal::Point_3, 3> points = {
        cgal_vertices[cgal_face[0]], cgal_vertices[cgal_face[1]],
        cgal_vertices[cgal_face[2]]};
    hypo::cgal::Vector_3 cgal_normal =
        CGAL::normal(points[0], points[1], points[2]);

    TriangleSoup::Vector3 normal = {
        static_cast<float>(CGAL::to_double(cgal_normal.x())),
        static_cast<float>(CGAL::to_double(cgal_normal.y())),
        static_cast<float>(CGAL::to_double(cgal_normal.z()))};

    for (const auto& point : points) {
      vertices.push_back(TriangleSoup::Vertex{
          {static_cast<float>(CGAL::to_double(point.x())),
           static_cast<float>(CGAL::to_double(point.y())),
           static_cast<float>(CGAL::to_double(point.z()))},
          normal,
      });
    }

    triangles.push_back(
        TriangleSoup::Triangle{static_cast<uint32_t>(vertices.size() - 3),
                               static_cast<uint32_t>(vertices.size() - 2),
                               static_cast<uint32_t>(vertices.size() - 1)});
  }

  return TriangleSoup{std::move(vertices), std::move(triangles)};
}

}  // namespace

reify::utils::ErrorOr<std::shared_ptr<
    reify::pure_cpp::SceneVisualizer<hypo::Region3, glm::mat4>::SceneObject>>
CreateSceneObjectRegion3(const hypo::Region3& data) {
  hypo::cgal::Nef_polyhedron_3 polyhedron3 = hypo::cgal::ConstructRegion3(data);
  const std::shared_ptr<const TriangleSoup> triangle_soup(
      new TriangleSoup(ConvertToTriangleSoup(polyhedron3)));

        return std::shared_ptr<reify::pure_cpp::SceneVisualizer<hypo::Region3, glm::mat4>::SceneObject>(
            new SceneObjectRegion3(std::move(polyhedron3), triangle_soup));
}

SceneObjectRegion3::SceneObjectRegion3(
    hypo::cgal::Nef_polyhedron_3&& polyhedron3,
    const std::shared_ptr<const TriangleSoup>& triangle_soup)
    : polyhedron3_(std::move(polyhedron3)), triangle_soup_(triangle_soup) {}

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

      hypo::cgal::ExportToSTL(polyhedron3_,
                              std::filesystem::absolute(selected_path));
      export_file_selector_->Close();
    }
    if (!export_file_selector_->IsOpened()) {
      export_file_selector_.reset();
    }
  }
}

reify::utils::ErrorOr<std::unique_ptr<reify::pure_cpp::SceneVisualizer<
    hypo::Region3, glm::mat4>::SceneObjectRenderable>>
SceneObjectRegion3::CreateSceneObjectRenderable(
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

  return std::unique_ptr<reify::pure_cpp::SceneVisualizer<
      hypo::Region3, glm::mat4>::SceneObjectRenderable>(
      new SceneObjectRenderableRegion3(std::move(mesh_renderer)));
}

reify::utils::ErrorOr<reify::window::Window::Renderer::FrameResources>
SceneObjectRenderableRegion3::Render(VkCommandBuffer command_buffer,
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
