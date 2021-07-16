#include "src/domain_visualizer/domain_visualizer_hypo.h"

#include <CGAL/boost/graph/convert_nef_polyhedron_to_polygon_mesh.h>

#include <thread>

// clang-format off
#include "imgui.h"
#include "imfilebrowser.h"
// clang-format on
#include <fmt/format.h>

#include "cgal/construct_region2.h"
#include "cgal/construct_region3.h"
#include "cgal/export_to_stl.h"
#include "cgal/types_nef_polyhedron_3.h"
#include "reify/purecpp/hypo.h"
#include "reify/typescript_cpp_v8/hypo.h"
#include "reify/typescript_cpp_v8/typescript_cpp_v8.h"

namespace {
std::unordered_map<std::string, TypeScriptSymbolVisualizer> ToTypeVisualizerMap(
    const std::vector<TypeScriptSymbolVisualizer>& visualizers) {
  std::unordered_map<std::string, TypeScriptSymbolVisualizer> result;
  for (const auto& visualizer : visualizers) {
    result.insert(std::make_pair(visualizer.typescript_type, visualizer));
  }
  return result;
}
}  // namespace

TypeScriptSymbolVisualizerStack::TypeScriptSymbolVisualizerStack(
    const std::vector<
        reify::typescript_cpp_v8::CompilerEnvironment::InputModule>&
        typescript_modules,
    const std::vector<TypeScriptSymbolVisualizer>& visualizers)
    : typescript_modules_(typescript_modules),
      typescript_type_to_visualizer_(ToTypeVisualizerMap(visualizers)) {}

const TypeScriptSymbolVisualizer*
TypeScriptSymbolVisualizerStack::FindVisualizerForSymbol(
    const reify::typescript_cpp_v8::CompiledModule::ExportedSymbol& symbol)
    const {
  for (const auto& item : typescript_type_to_visualizer_) {
    if (symbol.typescript_type_string == fmt::format("() => {}", item.first)) {
      return &item.second;
    }
  }
  return nullptr;
}

bool TypeScriptSymbolVisualizerStack::CanPreviewSymbol(
    const reify::typescript_cpp_v8::CompiledModule::ExportedSymbol& symbol) {
  return FindVisualizerForSymbol(symbol);
}

reify::utils::Future<TypeScriptSymbolVisualizerStack::ErrorOr<std::any>>
TypeScriptSymbolVisualizerStack::PrepareSymbolForPreview(
    std::shared_ptr<reify::typescript_cpp_v8::CompiledModule> module,
    const reify::typescript_cpp_v8::CompiledModule::ExportedSymbol& symbol) {
  return runtime_thread_.EnqueueWithResult<
      ErrorOr<std::any>>([this, module,
                          symbol = std::move(symbol)]() -> ErrorOr<std::any> {
    // Setup a V8 runtime environment around the CompiledModule.  This will
    // enable us to call exported functions and query exported values from
    // the module.
    auto runtime_env_or_error =
        reify::typescript_cpp_v8::CreateRuntimeEnvironment(module);
    if (auto error = std::get_if<0>(&runtime_env_or_error)) {
      return Error{*error};
    }

    reify::typescript_cpp_v8::RuntimeEnvironment runtime_env(
        std::move(std::get<1>(runtime_env_or_error)));

    auto visualizer = FindVisualizerForSymbol(symbol);
    if (!visualizer) {
      return Error{"Hypo's visualizer does not support this symbol type."};
    }

    auto results =
        visualizer
            ->prepare_symbol_for_preview_function(&runtime_env, module, symbol)
            .wait_and_get_results();

    if (auto canceled_future = std::get_if<0>(&results)) {
      return Error{"Cancelled."};
    }
    if (auto error = std::get_if<0>(&std::get<1>(results))) {
      return *error;
    }
    return PreparedSymbol{std::get<std::any>(std::get<1>(results)), visualizer};
  });
}

class TypeScriptSymbolVisualizerStack::Renderer
    : public reify::window::Window::Renderer {
 public:
  Renderer(std::unordered_map<
               std::string, std::unique_ptr<reify::window::Window::Renderer>>&&
               renderers,
           const std::function<void()>& on_destroy);
  ~Renderer() { on_destroy_(); }

  void SetCurrent(const std::optional<std::string>& current_renderer) {
    if (current_renderer) {
      auto found = renderers_.find(*current_renderer);
      assert(found != renderers_.end());
      current_renderer_ = found->second.get();
    } else {
      current_renderer_ = nullptr;
    }
  }

  ErrorOr<FrameResources> RenderFrame(
      VkCommandBuffer command_buffer, VkFramebuffer framebuffer,
      VkImage output_color_image,
      const reify::window::Rect& viewport_region) override {
    if (current_renderer_) {
      return current_renderer_->RenderFrame(
          command_buffer, framebuffer, output_color_image, viewport_region);
    } else {
      return FrameResources();
    }
  }

 private:
  const std::unordered_map<std::string,
                           std::unique_ptr<reify::window::Window::Renderer>>
      renderers_;
  const std::function<void()> on_destroy_;

  reify::window::Window::Renderer* current_renderer_ = nullptr;
};

void TypeScriptSymbolVisualizerStack::SetPreview(
    const std::any& prepared_symbol) {
  if (selected_symbol_) {
    selected_symbol_->associated_visualizer->set_preview_function(std::nullopt);
    if (renderer_) {
      renderer_->SetCurrent(std::nullopt);
    }
  }
  selected_symbol_ = std::any_cast<PreparedSymbol>(prepared_symbol);

  if (last_viewport_resize_) {
    selected_symbol_->associated_visualizer->window->OnViewportResize(
        *last_viewport_resize_);
  }

  selected_symbol_->associated_visualizer->set_preview_function(
      selected_symbol_->processed_data);
  if (renderer_) {
    renderer_->SetCurrent(
        selected_symbol_->associated_visualizer->typescript_type);
  }
}

void TypeScriptSymbolVisualizerStack::ClearPreview() {
  if (selected_symbol_) {
    selected_symbol_->associated_visualizer->set_preview_function(std::nullopt);
  }
  selected_symbol_ = std::nullopt;
}

bool TypeScriptSymbolVisualizerStack::OnInputEvent(
    const InputEvent& input_event) {
  if (selected_symbol_) {
    return selected_symbol_->associated_visualizer->window->OnInputEvent(
        input_event);
  } else {
    return false;
  }
}

void TypeScriptSymbolVisualizerStack::OnViewportResize(
    const std::array<int, 2>& size) {
  last_viewport_resize_ = size;
  if (selected_symbol_) {
    selected_symbol_->associated_visualizer->window->OnViewportResize(size);
  }
}

void TypeScriptSymbolVisualizerStack::AdvanceTime(
    std::chrono::duration<float> seconds) {
  if (selected_symbol_) {
    selected_symbol_->associated_visualizer->window->AdvanceTime(seconds);
  }
}

TypeScriptSymbolVisualizerStack::Renderer::Renderer(
    std::unordered_map<std::string,
                       std::unique_ptr<reify::window::Window::Renderer>>&&
        renderers,
    const std::function<void()>& on_destroy)
    : renderers_(std::move(renderers)), on_destroy_(on_destroy) {}

TypeScriptSymbolVisualizerStack::ErrorOr<
    std::unique_ptr<reify::window::Window::Renderer>>
TypeScriptSymbolVisualizerStack::CreateRenderer(
    VkInstance instance, VkPhysicalDevice physical_device, VkDevice device,
    VkFormat output_image_format) {
  std::unordered_map<std::string,
                     std::unique_ptr<reify::window::Window::Renderer>>
      renderers;
  for (const auto& item : typescript_type_to_visualizer_) {
    auto error_or_renderer = item.second.window->CreateRenderer(
        instance, physical_device, device, output_image_format);
    if (auto error = std::get_if<0>(&error_or_renderer)) {
      return *error;
    }
    renderers[item.first] = std::move(std::get<1>(error_or_renderer));
  }

  std::unique_ptr<Renderer> renderer(
      new Renderer(std::move(renderers), [this]() { renderer_ = nullptr; }));
  renderer_ = renderer.get();
  if (selected_symbol_) {
    renderer_->SetCurrent(
        selected_symbol_->associated_visualizer->typescript_type);
  }
  return std::move(renderer);
}

bool TypeScriptSymbolVisualizerStack::HasImGuiWindow() const {
  return selected_symbol_ &&
         selected_symbol_->associated_visualizer->im_gui_visualizer;
}

std::string TypeScriptSymbolVisualizerStack::ImGuiWindowPanelTitle() const {
  if (selected_symbol_) {
    return selected_symbol_->associated_visualizer->im_gui_visualizer
        ->ImGuiWindowPanelTitle();
  } else {
    assert(false);
    return "nothing!";
  }
}

void TypeScriptSymbolVisualizerStack::RenderImGuiWindow() {
  if (selected_symbol_) {
    selected_symbol_->associated_visualizer->im_gui_visualizer
        ->RenderImGuiWindow();
  }
}

ObjectVisualizerHypoRegion3::ObjectVisualizerHypoRegion3()
    : free_camera_viewport_(0, 0) {}

ObjectVisualizerHypoRegion3::~ObjectVisualizerHypoRegion3() {}

reify::utils::Future<
    reify::typescript_cpp_v8::DomainVisualizer::ErrorOr<std::any>>
ObjectVisualizerHypoRegion3::PrepareDataForPreview(const hypo::Region3& data) {
  return builder_thread_.EnqueueWithResult<
      reify::typescript_cpp_v8::DomainVisualizer::ErrorOr<std::any>>(
      [data]()
          -> reify::typescript_cpp_v8::DomainVisualizer::ErrorOr<std::any> {
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

void ObjectVisualizerHypoRegion3::SetPreview(
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

bool ObjectVisualizerHypoRegion3::OnInputEvent(const InputEvent& input_event) {
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

void ObjectVisualizerHypoRegion3::OnViewportResize(
    const std::array<int, 2>& size) {
  free_camera_viewport_.AccumulateViewportResize(size[0], size[1]);
}

void ObjectVisualizerHypoRegion3::AdvanceTime(
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

ObjectVisualizerHypoRegion3::ErrorOr<
    std::unique_ptr<ObjectVisualizerHypoRegion3::Renderer>>
ObjectVisualizerHypoRegion3::CreateRenderer(VkInstance instance,
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

  return std::make_unique<RendererRegion3>(
      std::move(mesh_renderer),
      [this]() { return free_camera_viewport_.ViewMatrix(); },
      [this]() { mesh_renderer_ = nullptr; });
}

std::string ObjectVisualizerHypoRegion3::ImGuiWindowPanelTitle() const {
  return "Region3 Options";
}

void ObjectVisualizerHypoRegion3::RenderImGuiWindow() {
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

HypoTypeScriptSymbolVisualizerStack::HypoTypeScriptSymbolVisualizerStack()
    : visualizer(reify::typescript_cpp_v8::hypo::typescript_declarations(),
                 {*MakeTypeScriptSymbolVisualizer(&region_3_visualizer)}) {}
