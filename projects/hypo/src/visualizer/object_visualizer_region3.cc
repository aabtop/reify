#include "src/visualizer/object_visualizer_region3.h"

#include <CGAL/boost/graph/convert_nef_polyhedron_to_polygon_mesh.h>

#include <thread>

// clang-format off
#include "imgui.h"
#include "imfilebrowser.h"
// clang-format on

#include "cgal/construct_region2.h"
#include "cgal/construct_region3.h"
#include "cgal/export_to_stl.h"
#include "cgal/types_nef_polyhedron_3.h"
#include "reify/purecpp/hypo.h"

namespace hypo {

ObjectVisualizerRegion3::ObjectVisualizerRegion3()
    : free_camera_viewport_(0, 0) {}

ObjectVisualizerRegion3::~ObjectVisualizerRegion3() {}

reify::utils::Future<reify::utils::ErrorOr<std::any>>
ObjectVisualizerRegion3::PrepareDataForPreview(const hypo::Region3& data) {
  return builder_thread_.EnqueueWithResult<reify::utils::ErrorOr<std::any>>(
      [data]() -> reify::utils::ErrorOr<std::any> {
        return std::shared_ptr<hypo::cgal::Nef_polyhedron_3>(
            new hypo::cgal::Nef_polyhedron_3(
                hypo::cgal::ConstructRegion3(data)));
      });
}

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

void ObjectVisualizerRegion3::SetPreview(
    const std::optional<std::any>& prepared_symbol) {
  if (!prepared_symbol) {
    current_preview_ = nullptr;

    if (mesh_renderer_) {
      mesh_renderer_->SetTriangleSoup(nullptr);
    } else {
      pending_triangle_soup_ = nullptr;
    }
  } else {
    current_preview_ =
        std::any_cast<std::shared_ptr<hypo::cgal::Nef_polyhedron_3>>(
            *prepared_symbol);
    auto triangle_soup = std::make_shared<TriangleSoup>(
        ConvertToTriangleSoup(*current_preview_));
    if (mesh_renderer_) {
      mesh_renderer_->SetTriangleSoup(triangle_soup);
    } else {
      pending_triangle_soup_ = triangle_soup;
    }
  }
}

bool ObjectVisualizerRegion3::OnInputEvent(const InputEvent& input_event) {
  if (auto event = std::get_if<MouseMoveEvent>(&input_event)) {
    free_camera_viewport_.AccumulateMouseMove(event->x, event->y);
  } else if (auto event = std::get_if<MouseButtonEvent>(&input_event)) {
    free_camera_viewport_.AccumulateMouseButtonEvent(
        event->button, event->pressed, event->x, event->y);
  } else if (auto event = std::get_if<MouseWheelEvent>(&input_event)) {
    free_camera_viewport_.AccumulateMouseWheelEvent(event->angle_in_degrees);
  } else if (auto event = std::get_if<KeyboardEvent>(&input_event)) {
    free_camera_viewport_.AccumulateKeyboardEvent(event->key, event->pressed);
  }

  return false;
}

void ObjectVisualizerRegion3::OnViewportResize(const std::array<int, 2>& size) {
  free_camera_viewport_.AccumulateViewportResize(size[0], size[1]);
}

void ObjectVisualizerRegion3::AdvanceTime(
    std::chrono::duration<float> seconds) {
  free_camera_viewport_.AccumulateTimeDelta(seconds);
}

namespace {

class RendererRegion3 : public reify::window::Window::Renderer {
 public:
  RendererRegion3(std::unique_ptr<MeshRenderer> mesh_renderer,
                  const std::function<glm::mat4()>& get_view_matrix,
                  const std::function<void()>& on_destroy)
      : mesh_renderer_(std::move(mesh_renderer)),
        get_view_matrix_(get_view_matrix),
        on_destroy_(on_destroy) {}
  ~RendererRegion3() { on_destroy_(); }

  reify::utils::ErrorOr<FrameResources> RenderFrame(
      VkCommandBuffer command_buffer, VkFramebuffer framebuffer,
      VkImage output_color_image,
      const reify::window::Rect& viewport_region) override {
    return mesh_renderer_->RenderFrame(
        command_buffer, framebuffer, output_color_image,
        {viewport_region.left, viewport_region.top, viewport_region.right,
         viewport_region.bottom},
        get_view_matrix_());
  }

 private:
  std::unique_ptr<MeshRenderer> mesh_renderer_;
  std::function<glm::mat4()> get_view_matrix_;
  std::function<void()> on_destroy_;
};

}  // namespace

reify::utils::ErrorOr<std::unique_ptr<ObjectVisualizerRegion3::Renderer>>
ObjectVisualizerRegion3::CreateRenderer(VkInstance instance,
                                        VkPhysicalDevice physical_device,
                                        VkDevice device,
                                        VkFormat output_image_format) {
  auto renderer_or_error = MeshRenderer::Create(instance, physical_device,
                                                device, output_image_format);
  if (auto error = std::get_if<0>(&renderer_or_error)) {
    return reify::utils::Error{error->msg};
  }

  auto mesh_renderer = std::unique_ptr<MeshRenderer>(
      new MeshRenderer(std::move(std::get<1>(renderer_or_error))));
  mesh_renderer_ = mesh_renderer.get();

  if (pending_triangle_soup_) {
    mesh_renderer_->SetTriangleSoup(pending_triangle_soup_);
    pending_triangle_soup_.reset();
  }

  return std::make_unique<RendererRegion3>(
      std::move(mesh_renderer),
      [this]() { return free_camera_viewport_.ViewMatrix(); },
      [this]() { mesh_renderer_ = nullptr; });
}

std::string ObjectVisualizerRegion3::ImGuiWindowPanelTitle() const {
  return "Region3 Options";
}

void ObjectVisualizerRegion3::RenderImGuiWindow() {
  if (ImGui::Button("Reset Camera")) {
    free_camera_viewport_.Reset();
  }
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

      hypo::cgal::ExportToSTL(*current_preview_,
                              std::filesystem::absolute(selected_path));
      export_file_selector_->Close();
    }
    if (!export_file_selector_->IsOpened()) {
      export_file_selector_.reset();
    }
  }
}

}  // namespace hypo
