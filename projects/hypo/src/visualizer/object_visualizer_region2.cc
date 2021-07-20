#include "src/visualizer/object_visualizer_region2.h"

#include <CGAL/Polygon_mesh_processing/polygon_mesh_to_polygon_soup.h>

#include <thread>

// clang-format off
#include "imgui.h"
#include "imfilebrowser.h"
// clang-format on

#include "cgal/construct_region2.h"
#include "cgal/embed_2d_in_3d.h"
#include "cgal/export_to_svg.h"
#include "cgal/types_polygons.h"
#include "reify/purecpp/hypo.h"

namespace hypo {

ObjectVisualizerRegion2::ObjectVisualizerRegion2() : camera_(0, 0) {}

ObjectVisualizerRegion2::~ObjectVisualizerRegion2() {}

reify::utils::Future<reify::utils::ErrorOr<std::any>>
ObjectVisualizerRegion2::PrepareDataForPreview(const hypo::Region2& data) {
  return builder_thread_.EnqueueWithResult<reify::utils::ErrorOr<std::any>>(
      [data]() -> reify::utils::ErrorOr<std::any> {
        return std::shared_ptr<hypo::cgal::Polygon_set_2>(
            new hypo::cgal::Polygon_set_2(hypo::cgal::ConstructRegion2(data)));
      });
}

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

void ObjectVisualizerRegion2::SetPreview(
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
        std::any_cast<std::shared_ptr<hypo::cgal::Polygon_set_2>>(
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

bool ObjectVisualizerRegion2::OnInputEvent(const InputEvent& input_event) {
  if (auto event = std::get_if<MouseMoveEvent>(&input_event)) {
    camera_.AccumulateMouseMove(event->x, event->y);
  } else if (auto event = std::get_if<MouseButtonEvent>(&input_event)) {
    camera_.AccumulateMouseButtonEvent(event->button, event->pressed, event->x,
                                       event->y);
  } else if (auto event = std::get_if<MouseWheelEvent>(&input_event)) {
    camera_.AccumulateMouseWheelEvent(event->angle_in_degrees, event->x,
                                      event->y);
  } else if (auto event = std::get_if<KeyboardEvent>(&input_event)) {
  }

  return false;
}

void ObjectVisualizerRegion2::OnViewportResize(const std::array<int, 2>& size) {
  camera_.AccumulateViewportResize(size[0], size[1]);
}

void ObjectVisualizerRegion2::AdvanceTime(
    std::chrono::duration<float> seconds) {}

namespace {

class RendererRegion2 : public reify::window::Window::Renderer {
 public:
  RendererRegion2(
      std::unique_ptr<MeshRenderer> mesh_renderer,
      const std::function<glm::mat4(int, int)>& get_projection_view_matrix,
      const std::function<void()>& on_destroy)
      : mesh_renderer_(std::move(mesh_renderer)),
        get_projection_view_matrix_(get_projection_view_matrix),
        on_destroy_(on_destroy) {}
  ~RendererRegion2() { on_destroy_(); }

  reify::utils::ErrorOr<FrameResources> RenderFrame(
      VkCommandBuffer command_buffer, VkFramebuffer framebuffer,
      VkImage output_color_image,
      const reify::window::Rect& viewport_region) override {
    return mesh_renderer_->RenderFrame(
        command_buffer, framebuffer, output_color_image,
        {viewport_region.left, viewport_region.top, viewport_region.right,
         viewport_region.bottom},
        glm::mat4(1.0f),
        get_projection_view_matrix_(viewport_region.width(),
                                    viewport_region.height()));
  }

 private:
  std::unique_ptr<MeshRenderer> mesh_renderer_;
  std::function<glm::mat4(int, int)> get_projection_view_matrix_;
  std::function<void()> on_destroy_;
};
}  // namespace

reify::utils::ErrorOr<std::unique_ptr<ObjectVisualizerRegion2::Renderer>>
ObjectVisualizerRegion2::CreateRenderer(VkInstance instance,
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

  return std::make_unique<RendererRegion2>(
      std::move(mesh_renderer),
      [this](int width, int height) {
        return camera_.ProjectionViewMatrix(width, height);
      },
      [this]() { mesh_renderer_ = nullptr; });
}

std::string ObjectVisualizerRegion2::ImGuiWindowPanelTitle() const {
  return "Region2 Options";
}

void ObjectVisualizerRegion2::RenderImGuiWindow() {
  if (ImGui::Button("Reset Camera")) {
    camera_.Reset();
  }
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

      hypo::cgal::ExportToSVG(*current_preview_,
                              std::filesystem::absolute(selected_path));
      export_file_selector_->Close();
    }
    if (!export_file_selector_->IsOpened()) {
      export_file_selector_.reset();
    }
  }
}

}  // namespace hypo
