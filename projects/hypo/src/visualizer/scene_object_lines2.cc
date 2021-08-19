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

void MergeLineSegmentSoups(SceneObjectLines2::LineSegmentSoup&& src,
                           SceneObjectLines2::LineSegmentSoup* dest) {
  SceneObjectLines2::LineSegmentSoup::Index base_index = dest->vertices.size();

  std::copy(src.vertices.begin(), src.vertices.end(),
            std::back_inserter(dest->vertices));

  dest->simplices.reserve(dest->simplices.size() + src.simplices.size());
  for (const auto& line_segment : src.simplices) {
    dest->simplices.push_back(SceneObjectLines2::LineSegmentSoup::Simplex{
        line_segment[0] + base_index, line_segment[1] + base_index});
  }
}

SceneObjectLines2::LineSegmentSoup ConvertToLineSegmentSoup(
    const cgal::Polygon_2& polygon) {
  SceneObjectLines2::LineSegmentSoup result;
  result.vertices.reserve(polygon.size());
  result.simplices.reserve(polygon.size());

  for (auto iter = polygon.vertices_begin(); iter != polygon.vertices_end();
       ++iter) {
    result.vertices.push_back(SceneObjectLines2::LineSegmentSoup::Vertex{
        SceneObjectLines2::LineSegmentSoup::Point{
            static_cast<float>(CGAL::to_double(iter->x())),
            static_cast<float>(CGAL::to_double(iter->y()))}});
  }
  for (size_t i = 0; i < result.vertices.size() - 1; ++i) {
    auto index = static_cast<SceneObjectLines2::LineSegmentSoup::Index>(i);
    result.simplices.push_back(
        SceneObjectLines2::LineSegmentSoup::Simplex{index, index + 1});
  }
  result.simplices.push_back(SceneObjectLines2::LineSegmentSoup::Simplex{
      static_cast<SceneObjectLines2::LineSegmentSoup::Index>(
          result.vertices.size() - 1),
      0});

  return result;
}

SceneObjectLines2::LineSegmentSoup ConvertToLineSegmentSoup(
    const cgal::Polygon_with_holes_2& polygon_with_holes) {
  SceneObjectLines2::LineSegmentSoup result =
      ConvertToLineSegmentSoup(polygon_with_holes.outer_boundary());
  for (auto iter = polygon_with_holes.holes_begin();
       iter != polygon_with_holes.holes_end(); ++iter) {
    const cgal::Polygon_2& hole = *iter;
    MergeLineSegmentSoups(ConvertToLineSegmentSoup(hole), &result);
  }
  return result;
}

SceneObjectLines2::LineSegmentSoup ConvertToLineSegmentSoup(
    const cgal::Polygon_set_2& polygon_set) {
  SceneObjectLines2::LineSegmentSoup result;

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
  const std::shared_ptr<const SceneObjectLines2::LineSegmentSoup>
      line_segment_soup(new SceneObjectLines2::LineSegmentSoup(
          ConvertToLineSegmentSoup(polygon_set)));

  return std::shared_ptr<reify::pure_cpp::SceneObject<glm::mat3>>(
      new SceneObjectLines2(std::move(polygon_set), line_segment_soup));
}

SceneObjectLines2::SceneObjectLines2(
    cgal::Polygon_set_2&& polygon_set,
    const std::shared_ptr<const LineSegmentSoup>& line_segment_soup)
    : polygon_set_(std::move(polygon_set)),
      line_segment_soup_(line_segment_soup) {}

SceneObjectLines2::~SceneObjectLines2() {}

std::string SceneObjectLines2::ImGuiWindowPanelTitle() const {
  return "Lines2";
}
void SceneObjectLines2::RenderImGuiWindow() {}

reify::utils::ErrorOr<
    std::unique_ptr<reify::pure_cpp::SceneObjectRenderable<glm::mat3>>>
SceneObjectLines2::CreateSceneObjectRenderable(VkInstance instance,
                                               VkPhysicalDevice physical_device,
                                               VkDevice device,
                                               VkFormat output_image_format,
                                               VkRenderPass render_pass) {
  auto renderer_or_error = SimpleSimplexRenderer2::Create(
      instance, physical_device, device, output_image_format, render_pass, 1);
  if (auto error = std::get_if<0>(&renderer_or_error)) {
    return reify::utils::Error{error->msg};
  }

  auto line_segment_renderer = std::unique_ptr<SimpleSimplexRenderer2>(
      new SimpleSimplexRenderer2(std::move(std::get<1>(renderer_or_error))));
  if (line_segment_soup_) {
    line_segment_renderer->SetSimplexSoup(*line_segment_soup_);
  } else {
    line_segment_renderer->ClearSimplexSoup();
  }

  return std::unique_ptr<reify::pure_cpp::SceneObjectRenderable<glm::mat3>>(
      new SceneObjectRenderableLines2(std::move(line_segment_renderer)));
}

reify::utils::ErrorOr<reify::window::Window::Renderer::FrameResources>
SceneObjectRenderableLines2::Render(VkCommandBuffer command_buffer,
                                    const glm::mat3& view_projection_matrix) {
  return line_renderer2_->RenderFrame(command_buffer, view_projection_matrix,
                                      glm::vec4(0.95, 0.95, 0.95, 1.0));
}

}  // namespace visualizer
}  // namespace hypo
