#include "cgal/embed_2d_in_3d.h"

#include "cgal/types.h"

namespace hypo {
namespace cgal {

namespace {
Nef_polyhedron_3 EmbedPolygonIn3DXYPlane(const Polygon_2& polygon) {
  std::vector<Point_3> point_3s;
  point_3s.reserve(polygon.size());
  for (auto iter = polygon.vertices_begin(); iter != polygon.vertices_end();
       ++iter) {
    point_3s.emplace_back(iter->x(), iter->y(), 0);
  }
  return Nef_polyhedron_3(point_3s.begin(), point_3s.end());
}

Nef_polyhedron_3 EmbedPolygonWithHolesIn3DXYPlane(
    const Polygon_with_holes_2& polygon_with_holes) {
  Nef_polyhedron_3 output(
      EmbedPolygonIn3DXYPlane(polygon_with_holes.outer_boundary()));
  for (auto iter = polygon_with_holes.holes_begin();
       iter != polygon_with_holes.holes_end(); ++iter) {
    output ^= EmbedPolygonIn3DXYPlane(*iter);
  }
  return output;
}
}  // namespace

Nef_polyhedron_3 EmbedPolygonSetIn3DXYPlane(const Polygon_set_2& polygon_set) {
  Nef_polyhedron_3 output;
  std::vector<Polygon_with_holes_2> polygons_with_holes;
  polygons_with_holes.reserve(polygon_set.number_of_polygons_with_holes());
  polygon_set.polygons_with_holes(std::back_inserter(polygons_with_holes));
  for (const auto& polygon_with_holes : polygons_with_holes) {
    output += EmbedPolygonWithHolesIn3DXYPlane(polygon_with_holes);
  }
  return output;
}

}  // namespace cgal
}  // namespace hypo
