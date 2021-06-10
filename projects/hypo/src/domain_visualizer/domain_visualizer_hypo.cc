#include "src/domain_visualizer/domain_visualizer_hypo.h"

#include <CGAL/boost/graph/convert_nef_polyhedron_to_polygon_mesh.h>

#include <thread>

#include "cgal/construct_region2.h"
#include "cgal/construct_region3.h"
#include "cgal/types_nef_polyhedron_3.h"
#include "reify/purecpp/hypo.h"
#include "reify/typescript_cpp_v8.h"
#include "reify/typescript_cpp_v8/hypo.h"

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

DomainVisualizerHypo::DomainVisualizerHypo() : free_camera_viewport_(0, 0) {}

std::vector<reify::CompilerEnvironment::InputModule>
DomainVisualizerHypo::GetTypeScriptModules() {
  return reify::typescript_cpp_v8::hypo::typescript_declarations();
}

bool DomainVisualizerHypo::CanPreviewSymbol(
    const reify::CompiledModule::ExportedSymbol& symbol) {
  return (/*symbol.HasType<reify::Function<hypo::Region2()>>() ||*/
          symbol.HasType<reify::Function<hypo::Region3()>>());
}

reify::utils::Future<
    DomainVisualizerHypo::ErrorOr<DomainVisualizerHypo::PreparedSymbol>>
DomainVisualizerHypo::PrepareSymbolForPreview(
    std::shared_ptr<reify::CompiledModule> module,
    const reify::CompiledModule::ExportedSymbol& symbol) {
  return builder_thread_.EnqueueWithResult<ErrorOr<PreparedSymbol>>(
      [module, symbol = std::move(symbol)]() -> ErrorOr<PreparedSymbol> {
        // Setup a V8 runtime environment around the CompiledModule.  This will
        // enable us to call exported functions and query exported values from
        // the module.
        auto runtime_env_or_error = reify::CreateRuntimeEnvironment(module);
        if (auto error = std::get_if<0>(&runtime_env_or_error)) {
          return Error{*error};
        }

        reify::RuntimeEnvironment runtime_env(
            std::move(std::get<1>(runtime_env_or_error)));

        if (symbol.HasType<reify::Function<hypo::Region3()>>()) {
          auto entry_point_or_error =
              runtime_env.GetExport<reify::Function<hypo::Region3()>>(
                  symbol.name);
          if (auto error = std::get_if<0>(&entry_point_or_error)) {
            return Error{"Problem finding entrypoint function: " + *error};
          }
          auto entry_point = &std::get<1>(entry_point_or_error);

          auto result_or_error = entry_point->Call();
          if (auto error = std::get_if<0>(&result_or_error)) {
            return Error{"Error running function: " + *error};
          }

          hypo::Region3 result = std::get<1>(result_or_error);

          hypo::cgal::Nef_polyhedron_3 polyhedron3 =
              hypo::cgal::ConstructRegion3(result);

          return std::shared_ptr<TriangleSoup>(
              new TriangleSoup(ConvertToTriangleSoup(polyhedron3)));
        } else {
          return Error{"Hypo's visualizer does not support this symbol type."};
        }
      });
}

void DomainVisualizerHypo::SetPreview(const PreparedSymbol& prepared_symbol) {
  auto triangle_soup =
      std::any_cast<std::shared_ptr<TriangleSoup>>(prepared_symbol);

  if (mesh_renderer_) {
    mesh_renderer_->SetTriangleSoup(triangle_soup);
  } else {
    pending_triangle_soup_ = triangle_soup;
  }
}

bool DomainVisualizerHypo::OnInputEvent(const InputEvent& input_event) {
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

void DomainVisualizerHypo::OnViewportResize(const std::array<int, 2>& size) {
  free_camera_viewport_.AccumulateViewportResize(size[0], size[1]);
}

void DomainVisualizerHypo::AdvanceTime(std::chrono::duration<float> seconds) {
  free_camera_viewport_.AccumulateTimeDelta(seconds);
}

namespace {

class RendererHypo
    : public reify::typescript_cpp_v8::DomainVisualizer::Renderer {
 public:
  RendererHypo(std::unique_ptr<MeshRenderer> mesh_renderer,
               const std::function<glm::mat4()>& get_view_matrix,
               const std::function<void()>& on_destroy)
      : mesh_renderer_(std::move(mesh_renderer)),
        get_view_matrix_(get_view_matrix),
        on_destroy_(on_destroy) {}
  ~RendererHypo() { on_destroy_(); }

  ErrorOr<FrameResources> RenderFrame(
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

DomainVisualizerHypo::ErrorOr<std::unique_ptr<DomainVisualizerHypo::Renderer>>
DomainVisualizerHypo::CreateRenderer(VkInstance instance,
                                     VkPhysicalDevice physical_device,
                                     VkDevice device,
                                     VkFormat output_image_format) {
  auto renderer_or_error = MeshRenderer::Create(instance, physical_device,
                                                device, output_image_format);
  if (auto error = std::get_if<0>(&renderer_or_error)) {
    return Error{error->msg};
  }

  auto mesh_renderer = std::unique_ptr<MeshRenderer>(
      new MeshRenderer(std::move(std::get<1>(renderer_or_error))));
  mesh_renderer_ = mesh_renderer.get();

  if (pending_triangle_soup_) {
    mesh_renderer_->SetTriangleSoup(pending_triangle_soup_);
    pending_triangle_soup_.reset();
  }

  return std::make_unique<RendererHypo>(
      std::move(mesh_renderer),
      [this]() { return free_camera_viewport_.ViewMatrix(); },
      [this]() { mesh_renderer_ = nullptr; });
}
