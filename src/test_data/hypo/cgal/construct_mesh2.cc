#include "cgal/construct_mesh2.h"

#include "cgal/types.h"
#include "hypo.h"

namespace hypo {
namespace cgal {

namespace {
Polygon_set_2 Make(const hypo::CircleAsPolygon& circle_as_polygon) {
  Polygon_2 polygon;
  polygon.container().reserve(circle_as_polygon.num_points);

  const float one_over_num_points = 1.0f / circle_as_polygon.num_points;
  for (int i = 0; i < circle_as_polygon.num_points; ++i) {
    const float angle = i * one_over_num_points;
    polygon.push_back(Point_2(circle_as_polygon.circle.center[0],
                              circle_as_polygon.circle.center[1]) +
                      circle_as_polygon.circle.radius *
                          Vector_2(cos(angle), sin(angle)));
  }

  return Polygon_set_2(polygon);
}
}  // namespace

std::shared_ptr<Polygon_set_2> ConstructMesh2(const hypo::Mesh2& mesh2) {
  if (auto circle_as_polygon_ptr =
          std::get_if<std::shared_ptr<hypo::CircleAsPolygon>>(&mesh2)) {
    auto circle_as_polygon = *circle_as_polygon_ptr;
    return std::make_shared<Polygon_set_2>(Make(*circle_as_polygon));
  } else if (auto rectangle_ptr =
                 std::get_if<std::shared_ptr<hypo::Rectangle>>(&mesh2)) {
    auto rectangle = *rectangle_ptr;
  }
  return std::shared_ptr<Polygon_set_2>();
}

}  // namespace cgal
}  // namespace hypo
