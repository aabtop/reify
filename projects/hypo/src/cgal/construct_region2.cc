#include "cgal/construct_region2.h"

#include <CGAL/create_offset_polygons_from_polygon_with_holes_2.h>
#include <CGAL/minkowski_sum_2.h>

#include <limits>

#include "cgal/conversions.h"
#include "cgal/types_polygons.h"
#include "reify/pure_cpp/cache.h"
#include "reify/purecpp/hypo.h"

namespace hypo {
namespace cgal {

using FutureRegion2 = reify::pure_cpp::ThreadPoolCacheRunner::Future<
    std::shared_ptr<const Polygon_set_2>>;
using FutureBoundary2 = reify::pure_cpp::ThreadPoolCacheRunner::Future<
    std::shared_ptr<const Polygon_set_2>>;

namespace {
Polygon_set_2 ConstructRegion2(reify::pure_cpp::ThreadPoolCacheRunner* runner,
                               const hypo::Polygon& polygon) {
  Polygon_2 polygon_out;
  polygon_out.container().reserve(polygon.path.size());

  for (auto point : polygon.path) {
    polygon_out.push_back(Point_2(point[0], point[1]));
  }

  if (polygon_out.orientation() == CGAL::NEGATIVE) {
    polygon_out.reverse_orientation();
  }

  return Polygon_set_2(polygon_out);
}

Polygon_set_2 ConstructRegion2(reify::pure_cpp::ThreadPoolCacheRunner* runner,
                               const hypo::CircleAsPolygon& circle_as_polygon) {
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

Polygon_set_2 ConstructRegion2(reify::pure_cpp::ThreadPoolCacheRunner* runner,
                               const hypo::Box2& rectangle) {
  Polygon_2 polygon;
  polygon.container().reserve(3);

  hypo::Vec2 bmin;
  bmin.fill(std::numeric_limits<float>::infinity());
  hypo::Vec2 bmax;
  bmax.fill(-std::numeric_limits<float>::infinity());
  for (size_t i = 0; i < 2; ++i) {
    bmin[i] = std::min(rectangle.corners[0][i], rectangle.corners[1][i]);
    bmax[i] = std::max(rectangle.corners[0][i], rectangle.corners[1][i]);
  }

  polygon.push_back(Point_2(bmin[0], bmin[1]));
  polygon.push_back(Point_2(bmax[0], bmin[1]));
  polygon.push_back(Point_2(bmax[0], bmax[1]));
  polygon.push_back(Point_2(bmin[0], bmax[1]));

  return Polygon_set_2(polygon);
}

Polygon_set_2 ConstructRegion2(reify::pure_cpp::ThreadPoolCacheRunner* runner,
                               const hypo::Transform2& x) {
  Aff_transformation_2 transform = ToAff_transformation_2(x.transform);

  std::shared_ptr<const Polygon_set_2> input_polygon_set =
      cgal::ConstructRegion2(runner, x.source).Get();

  std::vector<Polygon_with_holes_2> polygons_with_holes;
  polygons_with_holes.reserve(
      input_polygon_set->number_of_polygons_with_holes());
  input_polygon_set->polygons_with_holes(
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

Polygon_set_2 ConstructRegion2(reify::pure_cpp::ThreadPoolCacheRunner* runner,
                               const hypo::Union2& x) {
  std::vector<FutureRegion2> children;
  children.reserve(x.regions.size());
  for (const auto& region : x.regions) {
    children.push_back(cgal::ConstructRegion2(runner, region));
  }

  Polygon_set_2 result;
  for (auto& child : children) {
    result.join(*child.Get());
  }
  return result;
}

Polygon_set_2 ConstructRegion2(reify::pure_cpp::ThreadPoolCacheRunner* runner,
                               const hypo::Intersection2& x) {
  if (x.regions.empty()) {
    return Polygon_set_2();
  }

  std::vector<FutureRegion2> children;
  children.reserve(x.regions.size());
  for (const auto& region : x.regions) {
    children.push_back(cgal::ConstructRegion2(runner, region));
  }

  Polygon_set_2 result(*children[0].Get());
  for (auto iter = children.begin() + 1; iter != children.end(); ++iter) {
    result.intersection(*iter->Get());
  }
  return result;
}

Polygon_set_2 ConstructRegion2(reify::pure_cpp::ThreadPoolCacheRunner* runner,
                               const hypo::Difference2& x) {
  auto a = cgal::ConstructRegion2(runner, x.a);
  auto b = cgal::ConstructRegion2(runner, x.b);
  Polygon_set_2 result;
  result.difference(*a.Get(), *b.Get());
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

Polygon_set_2 ConstructRegion2(reify::pure_cpp::ThreadPoolCacheRunner* runner,
                               const hypo::MinkowskiSum2& x) {
  if (x.regions.empty()) {
    return Polygon_set_2();
  }

  std::vector<FutureRegion2> children;
  children.reserve(x.regions.size());
  for (const auto& region : x.regions) {
    children.push_back(cgal::ConstructRegion2(runner, region));
  }

  Polygon_set_2 result = *children[0].Get();
  for (size_t i = 1; i < children.size(); ++i) {
    result = MinkowskiSum(result, *children[i].Get());
  }
  return result;
}

Polygon_set_2 ConstructRegion2(reify::pure_cpp::ThreadPoolCacheRunner* runner,
                               const hypo::WidenBoundary2& x) {
  Polygon_set_2 contours = *ConstructBoundary2(runner, x.boundary).Get();
  std::vector<Polygon_with_holes_2> contour_polygons_with_holes;
  contour_polygons_with_holes.reserve(contours.number_of_polygons_with_holes());
  contours.polygons_with_holes(std::back_inserter(contour_polygons_with_holes));

  Polygon_set_2 result;

  for (const auto& polygon_with_holes : contour_polygons_with_holes) {
    Polygon_set_2 inner_offset;
    {
      auto inner_offset_polygons =
          CGAL::create_interior_skeleton_and_offset_polygons_with_holes_2(
              x.width, polygon_with_holes, Kernel());
      for (const auto& inner_polygon_with_holes : inner_offset_polygons) {
        inner_offset.insert(*inner_polygon_with_holes);
      }
    }

    Polygon_set_2 outer_offset_boundary;
    {
      auto outer_offset_polygons =
          CGAL::create_exterior_skeleton_and_offset_polygons_with_holes_2(
              x.width, polygon_with_holes.outer_boundary(), Kernel());
      for (const auto& outer_polygon_with_holes : outer_offset_polygons) {
        // The exterior skeleton returns a result where a generated frame
        // is created that bounds the exterior offset polygon, so we actually
        // want the inverse of this, so we grab the hole.
        for (auto hole_iter = outer_polygon_with_holes->holes_begin();
             hole_iter != outer_polygon_with_holes->holes_end(); ++hole_iter) {
          Polygon_2 hole = *hole_iter;
          hole.reverse_orientation();
          outer_offset_boundary.insert(hole);
        }
      }
    }

    // The create_exterior_skeleton_and_offset_polygons_with_holes_2 function
    // also doesn't properly deal with holes, so we have to go into those and
    // process them separately.
    Polygon_set_2 outer_offset_holes;
    for (auto hole_iter = polygon_with_holes.holes_begin();
         hole_iter != polygon_with_holes.holes_end(); ++hole_iter) {
      Polygon_2 hole = *hole_iter;
      hole.reverse_orientation();
      auto inner_offset_polygons =
          CGAL::create_interior_skeleton_and_offset_polygons_with_holes_2(
              x.width, hole, Kernel());
      Polygon_set_2 inner_offset;
      for (const auto& inner_polygon_with_holes : inner_offset_polygons) {
        outer_offset_holes.insert(*inner_polygon_with_holes);
      }
    }

    // Now reconstruct the expanded outer offset with its holes.
    Polygon_set_2 outer_offset;
    outer_offset.difference(outer_offset_boundary, outer_offset_holes);

    // Finally subtract the inner_offset from the outer_offset to get the
    // final widened region.
    Polygon_set_2 difference;
    difference.difference(outer_offset, inner_offset);

    result.join(difference);
  }

  return result;
}

Polygon_set_2 ConstructBoundary2(reify::pure_cpp::ThreadPoolCacheRunner* runner,
                                 const hypo::BoundaryOfRegion2& x) {
  return *hypo::cgal::ConstructRegion2(runner, x.region).Get();
}

}  // namespace

FutureRegion2 ConstructRegion2(reify::pure_cpp::ThreadPoolCacheRunner* runner,
                               const hypo::Region2& x) {
  return std::visit(
      [runner](const auto& y) {
        auto future = runner->MakeFutureWithoutCaching<Polygon_set_2>(
            &ConstructRegion2, y);
        future.Get();  // Unfortunately there appears to be race conditions in
                       // CGAL for 2D operations. I think. So adding this in
                       // makes all 2D operations single threaded. Check back in
                       // with CGAL when they update their code, if it's fixed
                       // remove this Get() and change
                       // "MakeFutureWithoutCaching" to "MakeFuture". I was able
                       // to reproduce by checking if all the circles appear in
                       // the twist_ring.ts CrossSection function. Same for
                       // Boundary below.
        return std::move(future);
      },
      static_cast<const hypo::Region2::AsVariant&>(x));
}

FutureBoundary2 ConstructBoundary2(
    reify::pure_cpp::ThreadPoolCacheRunner* runner, const hypo::Boundary2& x) {
  return std::visit(
      [runner](const auto& y) {
        auto future = runner->MakeFutureWithoutCaching<Polygon_set_2>(
            &ConstructBoundary2, y);
        future.Get();
        return std::move(future);
      },
      static_cast<const hypo::Boundary2::AsVariant&>(x));
}

}  // namespace cgal
}  // namespace hypo
