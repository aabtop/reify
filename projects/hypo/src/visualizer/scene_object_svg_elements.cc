#include "src/visualizer/scene_object_svg_elements.h"

// clang-format off
#include "imgui.h"
#include "imfilebrowser.h"
// clang-format on

#include <fmt/format.h>

#include <glm/glm.hpp>

#include "cgal/errors.h"
#include "reify/purecpp/hypo.h"
#include "svg/construct_svg.h"
#include "svg/export_to_svg.h"
#include "visualizer/construct_visualizer_svg.h"

namespace hypo {
namespace visualizer {

namespace {
glm::vec4 sRGBAToGlmVec4(const hypo::sRGBA& srgba) {
  return glm::vec4(srgba[0], srgba[1], srgba[2], srgba[3]);
}
}  // namespace

reify::utils::ErrorOr<std::shared_ptr<reify::pure_cpp::SceneObject<glm::mat3>>>
CreateSceneObjectSvgElements(reify::pure_cpp::ThreadPoolCacheRunner* runner,
                             const hypo::SvgElements& data,
                             const std::string& window_panel_title) {
  // Build the SVG elements into efficient structures for the purpose of
  // rendering them.
  REIFY_UTILS_ASSIGN_OR_RETURN(
      visualizer_svg_elements,
      hypo::cgal::CallCgalAndCatchExceptions(&ConstructVisualizerSvgElements,
                                             runner, data));
  // Also build the SVG elements for the purposes of exporting them, in case
  // that's something the user wants to do.
  REIFY_UTILS_ASSIGN_OR_RETURN(svg_elements,
                               hypo::cgal::CallCgalAndCatchExceptions(
                                   &svg::ConstructSvgElements, runner, data));

  return std::shared_ptr<reify::pure_cpp::SceneObject<glm::mat3>>(
      new SceneObjectSvgElements(
          visualizer_svg_elements, svg_elements,
          window_panel_title.empty() ? "SvgElements" : window_panel_title));
}

reify::utils::ErrorOr<std::shared_ptr<reify::pure_cpp::SceneObject<glm::mat3>>>
CreateSceneObjectSvgElement(reify::pure_cpp::ThreadPoolCacheRunner* runner,
                            const hypo::SvgElement& data,
                            const std::string& window_panel_title) {
  return CreateSceneObjectSvgElements(
      runner, hypo::SvgElements({{hypo::SvgElement(data)}}),
      window_panel_title.empty() ? "SvgElement" : window_panel_title);
}

SceneObjectSvgElements::SceneObjectSvgElements(
    const std::shared_ptr<const VisualizerSvgElements>& visualizer_svg_elements,
    const std::shared_ptr<const svg::Elements>& svg_elements,
    const std::string& window_panel_title)
    : svg_elements_(svg_elements),
      visualizer_svg_elements_(visualizer_svg_elements),
      window_panel_title_(window_panel_title) {}

SceneObjectSvgElements::~SceneObjectSvgElements() {}

std::string SceneObjectSvgElements::ImGuiWindowPanelTitle() const {
  return window_panel_title_;
}

void SceneObjectSvgElements::RenderImGuiWindow() {
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

      hypo::svg::ExportToSVG(*svg_elements_,
                             std::filesystem::absolute(selected_path));
      export_file_selector_->Close();
    }
    if (!export_file_selector_->IsOpened()) {
      export_file_selector_.reset();
    }
  }
}

reify::utils::ErrorOr<
    std::unique_ptr<reify::pure_cpp::SceneObjectRenderable<glm::mat3>>>
SceneObjectSvgElements::CreateSceneObjectRenderable(
    VkInstance instance, VkPhysicalDevice physical_device, VkDevice device,
    VkFormat output_image_format, VkRenderPass render_pass) {
  std::vector<SimpleSimplexRenderer2> renderers;
  renderers.reserve(visualizer_svg_elements_->size());
  for (const auto& svg_element : *visualizer_svg_elements_) {
    if (auto path_element =
            std::get_if<VisualizerSvgPathElement>(&(*svg_element))) {
      if (auto path_element_from_region2 =
              std::get_if<VisualizerSvgPathElementFromRegion2>(path_element)) {
        auto error_or_renderer = SimpleSimplexRenderer2::Create(
            instance, physical_device, device, output_image_format, render_pass,
            *path_element_from_region2->region_triangles,
            sRGBAToGlmVec4(
                std::get<hypo::SvgSolidColor>(path_element_from_region2->fill)
                    .color));
        if (auto error = std::get_if<0>(&error_or_renderer)) {
          return reify::utils::Error{error->msg};
        }
        renderers.push_back(std::move(std::get<1>(error_or_renderer)));
      } else if (auto path_element_from_boundary2 =
                     std::get_if<VisualizerSvgPathElementFromBoundary2>(
                         path_element)) {
        // Of course width can be defined in other ways, but for the purposes
        // of visualization, we should have converted all other units of
        // measurement, which essentially give width to the boundary, into
        // a region so that it can be handled by the region code.
        assert(std::holds_alternative<hypo::SvgInfinitesimal>(
            path_element_from_boundary2->width));

        auto error_or_renderer = SimpleSimplexRenderer2::Create(
            instance, physical_device, device, output_image_format, render_pass,
            *path_element_from_boundary2->boundary_line_segments,
            sRGBAToGlmVec4(std::get<hypo::SvgSolidColor>(
                               path_element_from_boundary2->stroke)
                               .color));
        if (auto error = std::get_if<0>(&error_or_renderer)) {
          return reify::utils::Error{error->msg};
        }
        renderers.push_back(std::move(std::get<1>(error_or_renderer)));
      } else {
        assert(false);  // Unknown SVG path element type.
      }
    } else {
      assert(false);  // Unknown SVG element type.
    }
  }

  return std::unique_ptr<reify::pure_cpp::SceneObjectRenderable<glm::mat3>>(
      new SceneObjectRenderableSvgElements(std::move(renderers)));
}

reify::utils::ErrorOr<reify::window::Window::Renderer::FrameResources>
SceneObjectRenderableSvgElements::Render(
    VkCommandBuffer command_buffer, const glm::mat3& view_projection_matrix) {
  std::vector<reify::window::Window::Renderer::FrameResources> frame_resources;

  for (auto& element_renderer : element_renderers_) {
    REIFY_UTILS_ASSIGN_OR_RETURN(
        element_renderer_frame_resources,
        element_renderer.RenderFrame(command_buffer, view_projection_matrix));
    frame_resources.push_back(element_renderer_frame_resources);
  }

  return frame_resources;
}

}  // namespace visualizer
}  // namespace hypo
