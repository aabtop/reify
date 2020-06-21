#include "cgal/construct_region2.h"

#include <CGAL/minkowski_sum_2.h>

#include <limits>

#include "cgal/conversions.h"
#include "cgal/types_polygons.h"
#include "hypo.h"

namespace hypo {
namespace cgal {

namespace {
Polygon_set_2 ConstructRegion2(const hypo::Region2& x) {
  return hypo::cgal::ConstructRegion2(x);
}

Polygon_set_2 ConstructRegion2(const hypo::CircleAsPolygon& circle_as_polygon) {
  Polygon_2 polygon;
  polygon.container().reserve(circle_as_polygon.num_points);

  const float two_pi_over_num_points = (2 * kPi) / circle_as_polygon.num_points;
  for (int i = 0; i < circle_as_polygon.num_points; ++i) {
    const float angle = i * two_pi_over_num_points;
    polygon.push_back(Point_2(circle_as_polygon.circle.center[0],
                              circle_as_polygon.circle.center[1]) +
                      circle_as_polygon.circle.radius *
                          Vector_2(cos(angle), sin(angle)));
  }

  return Polygon_set_2(polygon);
}

Polygon_set_2 ConstructRegion2(const hypo::Rectangle& rectangle) {
  Polygon_2 polygon;
  polygon.container().reserve(3);

  float left = std::min(std::get<0>(rectangle.points)[0],
                        std::get<1>(rectangle.points)[0]);
  float right = std::max(std::get<0>(rectangle.points)[0],
                         std::get<1>(rectangle.points)[0]);
  float bottom = std::min(std::get<0>(rectangle.points)[1],
                          std::get<1>(rectangle.points)[1]);
  float top = std::max(std::get<0>(rectangle.points)[1],
                       std::get<1>(rectangle.points)[1]);

  polygon.push_back(Point_2(left, bottom));
  polygon.push_back(Point_2(right, bottom));
  polygon.push_back(Point_2(right, top));
  polygon.push_back(Point_2(left, top));

  return Polygon_set_2(polygon);
}

Polygon_set_2 ConstructRegion2(const hypo::Transform2& x) {
  Aff_transformation_2 transform = ToAff_transformation_2(x.transform);

  Polygon_set_2 input_polygon_set(ConstructRegion2(x.source));

  std::vector<Polygon_with_holes_2> polygons_with_holes;
  polygons_with_holes.reserve(
      input_polygon_set.number_of_polygons_with_holes());
  input_polygon_set.polygons_with_holes(
      std::back_inserter(polygons_with_holes));

  Polygon_set_2 output;

  for (const auto& polygon_with_holes : polygons_with_holes) {
    std::vector<Polygon_2> output_holes;
    std::transform(polygon_with_holes.holes_begin(),
                   polygon_with_holes.holes_end(),
                   std::back_inserter(output_holes),
                   [&transform](const auto& hole) -> Polygon_2 {
                     return CGAL::transform(transform, hole);
                   });

    output.insert(Polygon_with_holes_2(
        CGAL::transform(transform, polygon_with_holes.outer_boundary()),
        output_holes.begin(), output_holes.end()));
  }

  return output;
}

Polygon_set_2 ConstructRegion2(const hypo::Union2& x) {
  Polygon_set_2 result;
  for (const auto& region : x.regions) {
    result.join(ConstructRegion2(region));
  }
  return result;
}

Polygon_set_2 ConstructRegion2(const hypo::Intersection2& x) {
  if (x.regions.empty()) {
    return Polygon_set_2();
  }

  Polygon_set_2 result(ConstructRegion2(x.regions[0]));
  for (auto iter = x.regions.begin() + 1; iter != x.regions.end(); ++iter) {
    result.intersection(ConstructRegion2(*iter));
  }
  return result;
}

Polygon_set_2 ConstructRegion2(const hypo::Difference2& x) {
  Polygon_set_2 result;
  result.difference(ConstructRegion2(x.a), ConstructRegion2(x.b));
  return result;
}

Polygon_set_2 MinkowskiSum(const Polygon_set_2& a, const Polygon_set_2& b) {
  Polygon_set_2 result;

  std::vector<Polygon_with_holes_2> polygons_with_holes_a;
  polygons_with_holes_a.reserve(a.number_of_polygons_with_holes());
  a.polygons_with_holes(std::back_inserter(polygons_with_holes_a));

  std::vector<Polygon_with_holes_2> polygons_with_holes_b;
  polygons_with_holes_b.reserve(b.number_of_polygons_with_holes());
  b.polygons_with_holes(std::back_inserter(polygons_with_holes_b));

  for (const auto& polygon_a : polygons_with_holes_a) {
    for (const auto& polygon_b : polygons_with_holes_b) {
      result.join(CGAL::minkowski_sum_2(polygon_a, polygon_b));
    }
  }

  return result;
}

Polygon_set_2 ConstructRegion2(const hypo::MinkowskiSum2& x) {
  if (x.regions.empty()) {
    return Polygon_set_2();
  }

  Polygon_set_2 result = ConstructRegion2(x.regions[0]);
  for (size_t i = 1; i < x.regions.size(); ++i) {
    result = MinkowskiSum(result, ConstructRegion2(x.regions[i]));
  }
  return result;
}

}  // namespace

Polygon_set_2 ConstructRegion2(const hypo::Region2& x) {
  if (auto obj_ptr =
          std::get_if<std::shared_ptr<const hypo::CircleAsPolygon>>(&x)) {
    return ConstructRegion2(**obj_ptr);
  } else if (auto obj_ptr =
                 std::get_if<std::shared_ptr<const hypo::Rectangle>>(&x)) {
    return ConstructRegion2(**obj_ptr);
  } else if (auto obj_ptr =
                 std::get_if<std::shared_ptr<const hypo::Transform2>>(&x)) {
    return ConstructRegion2(**obj_ptr);
  } else if (auto obj_ptr =
                 std::get_if<std::shared_ptr<const hypo::Union2>>(&x)) {
    return ConstructRegion2(**obj_ptr);
  } else if (auto obj_ptr =
                 std::get_if<std::shared_ptr<const hypo::Difference2>>(&x)) {
    return ConstructRegion2(**obj_ptr);
  } else if (auto obj_ptr =
                 std::get_if<std::shared_ptr<const hypo::Intersection2>>(&x)) {
    return ConstructRegion2(**obj_ptr);
  } else if (auto obj_ptr =
                 std::get_if<std::shared_ptr<const hypo::MinkowskiSum2>>(&x)) {
    return ConstructRegion2(**obj_ptr);
  }

  std::cerr << "Unhandled Region2 type." << std::endl;
  assert(false);
  return Polygon_set_2();
}

}  // namespace cgal
}  // namespace hypo
