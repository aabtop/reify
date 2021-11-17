#include "visualizer/construct_visualizer_svg.h"

#include <cgal/embed_2d_in_3d.h>

#include "cgal/construct_region2.h"
#include "svg/construct_svg.h"

namespace hypo {
namespace visualizer {

namespace {
TriangleSoup2 ConvertToTriangleSoup(
    const hypo::cgal::Polygon_set_2& polygon_set) {
  cgal::Constrained_Delaunay_triangulation_2 cdt =
      cgal::TriangulatePolygonSet(polygon_set);

  std::vector<TriangleSoup2::Simplex> triangles;
  triangles.reserve(cdt.number_of_faces());
  std::vector<TriangleSoup2::Vertex> vertices;
  vertices.reserve(cdt.number_of_vertices());

  for (auto iter = cdt.finite_faces_begin(); iter != cdt.finite_faces_end();
       ++iter) {
    cgal::Constrained_Delaunay_triangulation_2::Face_handle face = iter;
    if (!face->info().in_domain()) continue;

    for (int i = 0; i < 3; ++i) {
      const cgal::Point_2& pt = face->vertex(i)->point();

      vertices.push_back(
          TriangleSoup2::Vertex{{static_cast<float>(CGAL::to_double(pt.x())),
                                 static_cast<float>(CGAL::to_double(pt.y()))}});
    }
    triangles.push_back(
        TriangleSoup2::Simplex{static_cast<uint32_t>(vertices.size() - 1),
                               static_cast<uint32_t>(vertices.size() - 2),
                               static_cast<uint32_t>(vertices.size() - 3)});
  }

  return TriangleSoup2{std::move(vertices), std::move(triangles)};
}

void MergeLineSegmentSoups(LineSegmentSoup2&& src, LineSegmentSoup2* dest) {
  LineSegmentSoup2::Index base_index = dest->vertices.size();

  std::copy(src.vertices.begin(), src.vertices.end(),
            std::back_inserter(dest->vertices));

  dest->simplices.reserve(dest->simplices.size() + src.simplices.size());
  for (const auto& line_segment : src.simplices) {
    dest->simplices.push_back(LineSegmentSoup2::Simplex{
        line_segment[0] + base_index, line_segment[1] + base_index});
  }
}

LineSegmentSoup2 ConvertToLineSegmentSoup(const cgal::Polygon_2& polygon) {
  LineSegmentSoup2 result;
  result.vertices.reserve(polygon.size());
  result.simplices.reserve(polygon.size());

  for (auto iter = polygon.vertices_begin(); iter != polygon.vertices_end();
       ++iter) {
    result.vertices.push_back(LineSegmentSoup2::Vertex{LineSegmentSoup2::Point{
        static_cast<float>(CGAL::to_double(iter->x())),
        static_cast<float>(CGAL::to_double(iter->y()))}});
  }
  for (size_t i = 0; i < result.vertices.size() - 1; ++i) {
    auto index = static_cast<LineSegmentSoup2::Index>(i);
    result.simplices.push_back(LineSegmentSoup2::Simplex{index, index + 1});
  }
  result.simplices.push_back(LineSegmentSoup2::Simplex{
      static_cast<LineSegmentSoup2::Index>(result.vertices.size() - 1), 0});

  return result;
}

LineSegmentSoup2 ConvertToLineSegmentSoup(
    const cgal::Polygon_with_holes_2& polygon_with_holes) {
  LineSegmentSoup2 result =
      ConvertToLineSegmentSoup(polygon_with_holes.outer_boundary());
  for (auto iter = polygon_with_holes.holes_begin();
       iter != polygon_with_holes.holes_end(); ++iter) {
    const cgal::Polygon_2& hole = *iter;
    MergeLineSegmentSoups(ConvertToLineSegmentSoup(hole), &result);
  }
  return result;
}

LineSegmentSoup2 ConvertToLineSegmentSoup(
    const cgal::Polygon_set_2& polygon_set) {
  LineSegmentSoup2 result;

  std::vector<cgal::Polygon_with_holes_2> polygons_with_holes;
  polygons_with_holes.reserve(polygon_set.number_of_polygons_with_holes());
  polygon_set.polygons_with_holes(std::back_inserter(polygons_with_holes));
  for (const auto& polygon_with_holes : polygons_with_holes) {
    MergeLineSegmentSoups(ConvertToLineSegmentSoup(polygon_with_holes),
                          &result);
  }

  return result;
}

using FutureVisualizerSvgElement =
    reify::pure_cpp::ThreadPoolCacheRunner::Future<
        std::shared_ptr<const VisualizerSvgElement>>;

TriangleSoup2 ConstructTriangleSoup(
    reify::pure_cpp::ThreadPoolCacheRunner* runner, const hypo::Region2& x) {
  return ConvertToTriangleSoup(*cgal::ConstructRegion2(runner, x).Get());
}

LineSegmentSoup2 ConstructLineSegmentSoup(
    reify::pure_cpp::ThreadPoolCacheRunner* runner, const hypo::Boundary2& x) {
  return ConvertToLineSegmentSoup(*cgal::ConstructBoundary2(runner, x).Get());
}

VisualizerSvgPathElement ConstructVisualizerSvgPathElement(
    reify::pure_cpp::ThreadPoolCacheRunner* runner,
    const hypo::SvgPathElementFromRegion2& x) {
  auto triangle_soup =
      runner->MakeFuture<TriangleSoup2>(&ConstructTriangleSoup, x.region).Get();
  return VisualizerSvgPathElementFromRegion2{triangle_soup, x.fill};
}

VisualizerSvgPathElement ConstructVisualizerSvgPathElement(
    reify::pure_cpp::ThreadPoolCacheRunner* runner,
    const hypo::SvgPathElementFromBoundary2& x) {
  if (std::holds_alternative<hypo::SvgInfinitesimal>(x.width)) {
    auto line_segment_soup = runner
                                 ->MakeFuture<LineSegmentSoup2>(
                                     &ConstructLineSegmentSoup, x.boundary)
                                 .Get();
    return VisualizerSvgPathElementFromBoundary2{line_segment_soup, x.stroke,
                                                 x.width};
  } else {
    // All other width types imply a width, and so we return a region for that
    // since that's what we need to render widths.

    // Okay actually we only have one supported width unit type, `px` or pixels,
    // so right now that's pretty easy to handle.
    const auto& width_absolute = std::get<hypo::SvgAbsolute>(x.width);
    assert(width_absolute.units == hypo::SvgScalarUnitType::px);
    float diameter_in_hypo_units = width_absolute.value;

    auto triangle_soup = runner
                             ->MakeFuture<TriangleSoup2, hypo::Region2>(
                                 &ConstructTriangleSoup,
                                 reify::New(hypo::WidenBoundary2(
                                     {x.boundary, diameter_in_hypo_units / 2})))
                             .Get();
    return VisualizerSvgPathElementFromRegion2{
        triangle_soup, std::get<hypo::SvgSolidColor>(x.stroke)};
  }
}

VisualizerSvgElement ConstructVisualizerSvgElement(
    reify::pure_cpp::ThreadPoolCacheRunner* runner,
    const hypo::SvgPathElement& x) {
  return std::visit(
      [runner](const auto& y) {
        return ConstructVisualizerSvgPathElement(runner, y);
      },
      static_cast<const hypo::SvgPathElement::AsVariant&>(x));
}

FutureVisualizerSvgElement ConstructVisualizerSvgElementFuture(
    reify::pure_cpp::ThreadPoolCacheRunner* runner, const hypo::SvgElement& x) {
  return std::visit(
      [runner](const auto& y) {
        auto future = runner->MakeFutureWithoutCaching<VisualizerSvgElement>(
            &ConstructVisualizerSvgElement, y);
        future.Get();
        return std::move(future);
      },
      static_cast<const hypo::SvgElement::AsVariant&>(x));
}

VisualizerSvgElements ConstructVisualizerSvgElementsInternal(
    reify::pure_cpp::ThreadPoolCacheRunner* runner,
    const hypo::SvgElements& x) {
  std::vector<FutureVisualizerSvgElement> children;
  children.reserve(x.elements.size());
  for (const hypo::SvgElement& element : x.elements) {
    children.push_back(ConstructVisualizerSvgElementFuture(runner, element));
  }

  VisualizerSvgElements result;
  result.reserve(children.size());
  for (auto& child : children) {
    result.push_back(child.Get());
  }

  return result;
}

}  // namespace

FutureVisualizerSvgElements ConstructVisualizerSvgElements(
    reify::pure_cpp::ThreadPoolCacheRunner* runner,
    const hypo::SvgElements& x) {
  auto future = runner->MakeFutureWithoutCaching<VisualizerSvgElements>(
      &ConstructVisualizerSvgElementsInternal, x);
  future.Get();
  return future;
}

}  // namespace visualizer
}  // namespace hypo
