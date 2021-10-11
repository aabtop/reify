#include "cgal/construct_region3.h"

#include <CGAL/Nef_nary_union_3.h>
#include <CGAL/Polygon_mesh_processing/polygon_soup_to_polygon_mesh.h>
#include <CGAL/aff_transformation_tags.h>
#include <CGAL/minkowski_sum_3.h>

#include "cgal/conversions.h"
#include "cgal/embed_2d_in_3d.h"
#include "cgal/primitives_3d.h"
#include "cgal/subdivide.h"
#include "cgal/types_nef_polyhedron_3.h"
#include "construct_region2.h"
#include "reify/pure_cpp/cache.h"
#include "reify/purecpp/hypo.h"

namespace reify {
namespace pure_cpp {

template <>
int64_t EstimatedMemoryUsageInBytes(const hypo::cgal::Nef_polyhedron_3& x) {
  // Empirically discovered by recording before/after memory usage, but probably
  // wrong.
  int64_t estimated_memory_usage = 110851 * x.number_of_vertices();

  assert(estimated_memory_usage >= 0);
  return estimated_memory_usage;
}

}  // namespace pure_cpp
}  // namespace reify

namespace hypo {
namespace cgal {

using FutureRegion3 = reify::pure_cpp::ThreadPoolCacheRunner::Future<
    std::shared_ptr<const Nef_polyhedron_3>>;

namespace {
// Just a convenience function to avoid having to qualify the namespace.
FutureRegion3 ConstructRegion3(reify::pure_cpp::ThreadPoolCacheRunner* runner,
                               const hypo::Region3& x) {
  return hypo::cgal::ConstructRegion3(runner, x);
}

Nef_polyhedron_3 ConstructRegion3(
    reify::pure_cpp::ThreadPoolCacheRunner* runner,
    const hypo::TriangleList3& x) {
  Surface_mesh mesh;
  CGAL::Polygon_mesh_processing::polygon_soup_to_polygon_mesh(
      x.vertices, x.triangles, mesh);
  return Nef_polyhedron_3(mesh);
}

Nef_polyhedron_3 ConstructRegion3(
    reify::pure_cpp::ThreadPoolCacheRunner* runner, const hypo::Box3& cuboid) {
  Nef_polyhedron_3 result(MakeUnitBoxMesh());

  hypo::Vec3 bmin;
  bmin.fill(std::numeric_limits<float>::infinity());
  hypo::Vec3 bmax;
  bmax.fill(-std::numeric_limits<float>::infinity());
  for (size_t i = 0; i < 3; ++i) {
    bmin[i] = std::min(cuboid.corners[0][i], cuboid.corners[1][i]);
    bmax[i] = std::max(cuboid.corners[0][i], cuboid.corners[1][i]);
  }
  hypo::Vec3 size{bmax[0] - bmin[0], bmax[1] - bmin[1], bmax[2] - bmin[2]};

  Aff_transformation_3 size_scale(size[0], 0, 0, 0, size[1], 0, 0, 0, size[2]);
  Aff_transformation_3 corner_translate(CGAL::Translation(),
                                        Vector_3(bmin[0], bmin[1], bmin[2]));
  result.transform(corner_translate * size_scale);

  return result;
}

Nef_polyhedron_3 ConstructRegion3(
    reify::pure_cpp::ThreadPoolCacheRunner* runner, const hypo::Extrude& x) {
  return EmbedPolygonSetAs3DSurfaceMesh(
      *ConstructRegion2(runner, x.source).Get(), x.transforms, x.closed);
}

Nef_polyhedron_3 ConstructRegion3(
    reify::pure_cpp::ThreadPoolCacheRunner* runner, const hypo::Transform3& x) {
  Nef_polyhedron_3 result(*ConstructRegion3(runner, x.source).Get());
  result.transform(ToAff_transformation_3(x.transform));
  return result;
}

Nef_polyhedron_3 ConstructRegion3(
    reify::pure_cpp::ThreadPoolCacheRunner* runner, const hypo::Union3& x) {
  if (x.regions.size() <= 3) {
    std::vector<FutureRegion3> children;
    children.reserve(x.regions.size());
    for (const auto& region : x.regions) {
      children.push_back(ConstructRegion3(runner, region));
    }

    if (x.regions.size() == 2) {
      return *children[0].Get() + *children[1].Get();
    } else {
      CGAL::Nef_nary_union_3<Nef_polyhedron_3> nef_nary_union;
      for (auto& child : children) {
        nef_nary_union.add_polyhedron(*child.Get());
      }
      return nef_nary_union.get_union();
    }
  } else {
    auto region3_half_1 = reify::New(hypo::Union3{std::vector<Region3>{
        x.regions.begin(), x.regions.begin() + x.regions.size() / 2}});
    auto region3_half_2 = reify::New(hypo::Union3{std::vector<Region3>{
        x.regions.begin() + x.regions.size() / 2, x.regions.end()}});

    auto built_half_1 = runner->MakeFutureWithoutCaching<Nef_polyhedron_3>(
        ConstructRegion3, region3_half_1);
    auto built_half_2 = runner->MakeFutureWithoutCaching<Nef_polyhedron_3>(
        ConstructRegion3, region3_half_2);

    return *built_half_1.Get() + *built_half_2.Get();
  }
}

Nef_polyhedron_3 ConstructRegion3(
    reify::pure_cpp::ThreadPoolCacheRunner* runner,
    const hypo::Intersection3& x) {
  if (x.regions.empty()) {
    return Nef_polyhedron_3();
  }

  std::vector<FutureRegion3> children;
  children.reserve(x.regions.size());
  for (const auto& region : x.regions) {
    children.push_back(ConstructRegion3(runner, region));
  }

  Nef_polyhedron_3 result(*children[0].Get());
  for (auto iter = children.begin() + 1; iter != children.end(); ++iter) {
    result *= *iter->Get();
  }
  return result;
}

Nef_polyhedron_3 ConstructRegion3(
    reify::pure_cpp::ThreadPoolCacheRunner* runner,
    const hypo::Difference3& x) {
  auto a = ConstructRegion3(runner, x.a);
  auto b = ConstructRegion3(runner, x.b);
  return *a.Get() - *b.Get();
}

Nef_polyhedron_3 ConstructRegion3(
    reify::pure_cpp::ThreadPoolCacheRunner* runner,
    const hypo::Icosahedron& x) {
  Nef_polyhedron_3 result(MakeUnitIcosahedronMesh());
  Aff_transformation_3 radius_scale(CGAL::Scaling(), x.sphere.radius);
  Aff_transformation_3 center_translate(
      CGAL::Translation(),
      Vector_3(x.sphere.center[0], x.sphere.center[1], x.sphere.center[2]));
  result.transform(center_translate * radius_scale);
  return result;
}

Nef_polyhedron_3 ConstructRegion3(
    reify::pure_cpp::ThreadPoolCacheRunner* runner, const hypo::Octahedron& x) {
  Nef_polyhedron_3 result(MakeUnitOctahedronMesh());
  Aff_transformation_3 radius_scale(CGAL::Scaling(), x.sphere.radius);
  Aff_transformation_3 center_translate(
      CGAL::Translation(),
      Vector_3(x.sphere.center[0], x.sphere.center[1], x.sphere.center[2]));
  result.transform(center_translate * radius_scale);
  return result;
}

Nef_polyhedron_3 ConstructRegion3(
    reify::pure_cpp::ThreadPoolCacheRunner* runner, const hypo::Subdivide& x) {
  return cgal::Subdivide(*ConstructRegion3(runner, x.source).Get(), x.method,
                         x.iterations);
}

// Given a SphereBased object, extracts from it the Sphere object.
const Sphere& ToSphere(const hypo::SphereBased& sphere_like) {
  return std::visit(
      [](const auto& x) -> const Sphere& {
        using T = std::decay_t<decltype(x)>;
        if constexpr (std::is_same<T,
                                   ::reify::Reference<
                                       const hypo::SubdivideSphere>>::value) {
          return ToSphere(x->source);
        } else {
          return x->sphere;
        }
      },
      static_cast<const hypo::SphereBased::AsVariant&>(sphere_like));
}

Nef_polyhedron_3 ConstructRegion3(
    reify::pure_cpp::ThreadPoolCacheRunner* runner,
    const hypo::SubdivideSphere& x) {
  return cgal::SubdivideSphere(
      std::visit(
          [runner](const auto& arg) {
            return *ConstructRegion3(runner, arg).Get();
          },
          static_cast<const hypo::SphereBased::AsVariant&>(x.source)),
      ToSphere(x.source), x.iterations);
}

Nef_polyhedron_3 ConstructRegion3(
    reify::pure_cpp::ThreadPoolCacheRunner* runner,
    const hypo::MinkowskiSum3& x) {
  if (x.regions.empty()) {
    return Nef_polyhedron_3();
  }

  std::vector<FutureRegion3> children;
  children.reserve(x.regions.size());
  for (const auto& region : x.regions) {
    children.push_back(ConstructRegion3(runner, region));
  }

  Nef_polyhedron_3 result(*children[0].Get());
  for (size_t i = 1; i < children.size(); ++i) {
    Nef_polyhedron_3 next(*children[i].Get());
    result = CGAL::minkowski_sum_3(result, next);
  }
  return result;
}

}  // namespace

FutureRegion3 ConstructRegion3(reify::pure_cpp::ThreadPoolCacheRunner* runner,
                               const hypo::Region3& x) {
  return std::visit(
      [runner](const auto& y) {
        return runner->MakeFuture<Nef_polyhedron_3>(&ConstructRegion3, y);
      },
      static_cast<const hypo::Region3::AsVariant&>(x));
}

}  // namespace cgal
}  // namespace hypo