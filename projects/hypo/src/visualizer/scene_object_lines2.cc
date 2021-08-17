#include "src/visualizer/scene_object_lines2.h"

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
namespace visualizer {

namespace {

void MergeLineSegmentSoups(LineRenderer2::LineSegmentSoup&& src,
                           LineRenderer2::LineSegmentSoup* dest) {
  LineRenderer2::LineSegmentSoup::Index base_index = dest->vertices.size();

  std::copy(src.vertices.begin(), src.vertices.end(),
            std::back_inserter(dest->vertices));

  dest->line_segments.reserve(dest->line_segments.size() +
                              src.line_segments.size());
  for (const auto& line_segment : src.line_segments) {
    dest->line_segments.push_back(LineRenderer2::LineSegmentSoup::LineSegment{
        line_segment[0] + base_index, line_segment[1] + base_index});
  }
}

LineRenderer2::LineSegmentSoup ConvertToLineSegmentSoup(
    const cgal::Polygon_2& polygon) {
  LineRenderer2::LineSegmentSoup result;
  result.vertices.reserve(polygon.size());
  result.line_segments.reserve(polygon.size());

  for (auto iter = polygon.vertices_begin(); iter != polygon.vertices_end();
       ++iter) {
    result.vertices.push_back(LineRenderer2::LineSegmentSoup::Vertex{
        LineRenderer2::LineSegmentSoup::Vector2{
            static_cast<float>(CGAL::to_double(iter->x())),
            static_cast<float>(CGAL::to_double(iter->y()))}});
  }
  for (size_t i = 0; i < result.vertices.size() - 1; ++i) {
    auto index = static_cast<LineRenderer2::LineSegmentSoup::Index>(i);
    result.line_segments.push_back(
        LineRenderer2::LineSegmentSoup::LineSegment{index, index + 1});
  }
  result.line_segments.push_back(LineRenderer2::LineSegmentSoup::LineSegment{
      static_cast<LineRenderer2::LineSegmentSoup::Index>(
          result.vertices.size() - 1),
      0});

  return result;
}

LineRenderer2::LineSegmentSoup ConvertToLineSegmentSoup(
    const cgal::Polygon_with_holes_2& polygon_with_holes) {
  LineRenderer2::LineSegmentSoup result =
      ConvertToLineSegmentSoup(polygon_with_holes.outer_boundary());
  for (auto iter = polygon_with_holes.holes_begin();
       iter != polygon_with_holes.holes_end(); ++iter) {
    const cgal::Polygon_2& hole = *iter;
    MergeLineSegmentSoups(ConvertToLineSegmentSoup(hole), &result);
  }
  return result;
}

LineRenderer2::LineSegmentSoup ConvertToLineSegmentSoup(
    const cgal::Polygon_set_2& polygon_set) {
  LineRenderer2::LineSegmentSoup result;

  std::vector<cgal::Polygon_with_holes_2> polygons_with_holes;
  polygons_with_holes.reserve(polygon_set.number_of_polygons_with_holes());
  polygon_set.polygons_with_holes(std::back_inserter(polygons_with_holes));
  for (const auto& polygon_with_holes : polygons_with_holes) {
    MergeLineSegmentSoups(ConvertToLineSegmentSoup(polygon_with_holes),
                          &result);
  }

  return result;
}
}  // namespace

reify::utils::ErrorOr<std::shared_ptr<reify::pure_cpp::SceneObject<glm::mat3>>>
CreateSceneObjectLines2(const hypo::Region2& data) {
  cgal::Polygon_set_2 polygon_set = cgal::ConstructRegion2(data);
  const std::shared_ptr<const LineRenderer2::LineSegmentSoup> line_segment_soup(
      new LineRenderer2::LineSegmentSoup(
          ConvertToLineSegmentSoup(polygon_set)));

  return std::shared_ptr<reify::pure_cpp::SceneObject<glm::mat3>>(
      new SceneObjectLines2(std::move(polygon_set), line_segment_soup));
}

SceneObjectLines2::SceneObjectLines2(
    cgal::Polygon_set_2&& polygon_set,
    const std::shared_ptr<const LineRenderer2::LineSegmentSoup>&
        line_segment_soup)
    : polygon_set_(std::move(polygon_set)),
      line_segment_soup_(line_segment_soup) {}

SceneObjectLines2::~SceneObjectLines2() {}

std::string SceneObjectLines2::ImGuiWindowPanelTitle() const {
  return "Lines2";
}
void SceneObjectLines2::RenderImGuiWindow() {
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

      cgal::ExportToSVG(polygon_set_, std::filesystem::absolute(selected_path));
      export_file_selector_->Close();
    }
    if (!export_file_selector_->IsOpened()) {
      export_file_selector_.reset();
    }
  }
}

reify::utils::ErrorOr<
    std::unique_ptr<reify::pure_cpp::SceneObjectRenderable<glm::mat3>>>
SceneObjectLines2::CreateSceneObjectRenderable(VkInstance instance,
                                               VkPhysicalDevice physical_device,
                                               VkDevice device,
                                               VkFormat output_image_format,
                                               VkRenderPass render_pass) {
  auto renderer_or_error = LineRenderer2::Create(
      instance, physical_device, device, output_image_format, render_pass);
  if (auto error = std::get_if<0>(&renderer_or_error)) {
    return reify::utils::Error{error->msg};
  }

  auto flag_line_segment_renderer = std::unique_ptr<LineRenderer2>(
      new LineRenderer2(std::move(std::get<1>(renderer_or_error))));
  flag_line_segment_renderer->SetLineSegmentSoup(line_segment_soup_);

  return std::unique_ptr<reify::pure_cpp::SceneObjectRenderable<glm::mat3>>(
      new SceneObjectRenderableLines2(std::move(flag_line_segment_renderer)));
}

reify::utils::ErrorOr<reify::window::Window::Renderer::FrameResources>
SceneObjectRenderableLines2::Render(VkCommandBuffer command_buffer,
                                    const glm::mat3& view_projection_matrix) {
  return line_renderer2_->RenderFrame(command_buffer, view_projection_matrix);
}

}  // namespace visualizer
}  // namespace hypo
