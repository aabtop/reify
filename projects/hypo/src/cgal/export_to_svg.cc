#include "cgal/export_to_svg.h"

#include <fstream>

#include "cgal/types_polygons.h"

namespace hypo {
namespace cgal {

namespace {

void OutputPoint(std::ostream* out, const Point_2& point) {
  *out << point.x() << "," << point.y();
}

void OutputPolygon(std::ostream* out, const Polygon_2& polygon,
                   bool reversed = false) {
  if (polygon.size() < 3) {
    return;
  }

  auto begin = polygon.vertices_begin();
  auto end = polygon.vertices_end();
  std::optional<std::vector<Point_2>> reversed_vertices;
  if (reversed) {
    reversed_vertices.emplace();
    reversed_vertices->reserve(polygon.size());
    std::reverse_copy(begin, end, std::back_inserter(*reversed_vertices));
    begin = reversed_vertices->begin();
    end = reversed_vertices->end();
  }

  *out << "M ";
  OutputPoint(out, *polygon.vertices_begin());
  *out << " L ";

  for (auto iter = polygon.vertices_begin() + 1; iter != polygon.vertices_end();
       ++iter) {
    const Point_2& point = *iter;
    OutputPoint(out, point);
    *out << " ";
  }

  *out << "Z";
}

void OutputPolygonWithHoles(std::ostream* out,
                            const Polygon_with_holes_2& polygon_with_holes) {
  *out << "  <path d=\"";
  OutputPolygon(out, polygon_with_holes.outer_boundary());

  for (auto iter = polygon_with_holes.holes_begin();
       iter != polygon_with_holes.holes_end(); ++iter) {
    *out << std::endl;
    *out << "           ";
    OutputPolygon(out, *iter, true);
  }

  *out << "\" />" << std::endl;
}

Bbox_2 BoundingBox(
    const std::vector<Polygon_with_holes_2>& polygons_with_holes) {
  Bbox_2 ret;
  for (const auto& polygon_with_holes : polygons_with_holes) {
    ret += polygon_with_holes.bbox();
  }
  return ret;
}
}  // namespace

bool ExportToSVG(const Polygon_set_2& polygon_set,
                 const std::string& output_filepath) {
  std::ofstream out(output_filepath);

  std::vector<Polygon_with_holes_2> polygons_with_holes;
  polygons_with_holes.reserve(polygon_set.number_of_polygons_with_holes());
  polygon_set.polygons_with_holes(std::back_inserter(polygons_with_holes));
  Bbox_2 bounding_box = BoundingBox(polygons_with_holes);

  out << "<svg version=\"1.1\" xmlns=\"http://www.w3.org/2000/svg\""
      << std::endl;
  out << "             viewBox=\"" << bounding_box.xmin() << " "
      << bounding_box.ymin() << " " << bounding_box.xmax() - bounding_box.xmin()
      << " " << bounding_box.ymax() - bounding_box.ymin() << "\">" << std::endl;

  out << "   <!-- Setup coordinate space such that positive y is up. -->"
      << std::endl;
  out << "  <g transform=\"translate(0," << bounding_box.ymax() << ")\">";
  out << "<g transform=\"scale(1, -1)\">";
  out << "<g transform=\"translate(0," << -bounding_box.ymin() << ")\">"
      << std::endl;
  out << std::endl;

  for (const auto& polygon_with_holes : polygons_with_holes) {
    OutputPolygonWithHoles(&out, polygon_with_holes);
  }

  out << std::endl;
  out << "  </g></g></g>" << std::endl;

  out << "</svg>" << std::endl;
  return true;
}

}  // namespace cgal
}  // namespace hypo
