#include "cgal/construct_region3.h"

#include <CGAL/Polygon_mesh_processing/polygon_soup_to_polygon_mesh.h>
#include <CGAL/aff_transformation_tags.h>
#include <CGAL/minkowski_sum_3.h>

#include "cgal/conversions.h"
#include "cgal/embed_2d_in_3d.h"
#include "cgal/primitives_3d.h"
#include "cgal/subdivide.h"
#include "cgal/types_nef_polyhedron_3.h"
#include "construct_region2.h"
#include "reify/purecpp/hypo.h"

namespace hypo {
namespace cgal {

namespace {
Nef_polyhedron_3 ConstructRegion3(const hypo::Region3& x) {
  return hypo::cgal::ConstructRegion3(x);
}

Nef_polyhedron_3 ConstructRegion3(const hypo::TriangleList3& x) {
  Surface_mesh mesh;
  CGAL::Polygon_mesh_processing::polygon_soup_to_polygon_mesh(
      x.vertices, x.triangles, mesh);
  return Nef_polyhedron_3(mesh);
}

Nef_polyhedron_3 ConstructRegion3(const hypo::Box3& cuboid) {
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

Nef_polyhedron_3 ConstructRegion3(const hypo::Extrude& x) {
  return EmbedPolygonSetAs3DSurfaceMesh(ConstructRegion2(x.source),
                                        x.transforms, x.closed);
}

Nef_polyhedron_3 ConstructRegion3(const hypo::Transform3& x) {
  Nef_polyhedron_3 result(ConstructRegion3(x.source));
  result.transform(ToAff_transformation_3(x.transform));
  return result;
}

Nef_polyhedron_3 ConstructRegion3(const hypo::Union3& x) {
  Nef_polyhedron_3 result;
  for (const auto& region : x.regions) {
    result += ConstructRegion3(region);
  }
  return result;
}

Nef_polyhedron_3 ConstructRegion3(const hypo::Intersection3& x) {
  if (x.regions.empty()) {
    return Nef_polyhedron_3();
  }

  Nef_polyhedron_3 result(ConstructRegion3(x.regions[0]));
  for (auto iter = x.regions.begin() + 1; iter != x.regions.end(); ++iter) {
    result *= (ConstructRegion3(*iter));
  }
  return result;
}

Nef_polyhedron_3 ConstructRegion3(const hypo::Difference3& x) {
  return ConstructRegion3(x.a) - ConstructRegion3(x.b);
}

Nef_polyhedron_3 ConstructRegion3(const hypo::Icosahedron& x) {
  Nef_polyhedron_3 result(MakeUnitIcosahedronMesh());
  Aff_transformation_3 radius_scale(CGAL::Scaling(), x.sphere.radius);
  Aff_transformation_3 center_translate(
      CGAL::Translation(),
      Vector_3(x.sphere.center[0], x.sphere.center[1], x.sphere.center[2]));
  result.transform(center_translate * radius_scale);
  return result;
}

Nef_polyhedron_3 ConstructRegion3(const hypo::Octahedron& x) {
  Nef_polyhedron_3 result(MakeUnitOctahedronMesh());
  Aff_transformation_3 radius_scale(CGAL::Scaling(), x.sphere.radius);
  Aff_transformation_3 center_translate(
      CGAL::Translation(),
      Vector_3(x.sphere.center[0], x.sphere.center[1], x.sphere.center[2]));
  result.transform(center_translate * radius_scale);
  return result;
}

Nef_polyhedron_3 ConstructRegion3(const hypo::Subdivide& x) {
  return cgal::Subdivide(ConstructRegion3(x.source), x.method, x.iterations);
}

// Given a SphereBased object, extracts from it the Sphere object.
const Sphere& ToSphere(const hypo::SphereBased& sphere_like) {
  return std::visit(
      [](const auto& x) -> const Sphere& {
        using T = std::decay_t<decltype(x)>;
        if constexpr (std::is_same<T,
                                   std::shared_ptr<
                                       const hypo::SubdivideSphere>>::value) {
          return ToSphere(x->source);
        } else {
          return x->sphere;
        }
      },
      static_cast<const hypo::SphereBased::AsVariant&>(sphere_like));
}

Nef_polyhedron_3 ConstructRegion3(const hypo::SubdivideSphere& x) {
  return cgal::SubdivideSphere(
      std::visit([](const auto& arg) { return ConstructRegion3(arg); },
                 static_cast<const hypo::SphereBased::AsVariant&>(x.source)),
      ToSphere(x.source), x.iterations);
}

Nef_polyhedron_3 ConstructRegion3(const hypo::MinkowskiSum3& x) {
  if (x.regions.empty()) {
    return Nef_polyhedron_3();
  }

  Nef_polyhedron_3 result(ConstructRegion3(x.regions[0]));
  for (size_t i = 1; i < x.regions.size(); ++i) {
    Nef_polyhedron_3 next(ConstructRegion3(x.regions[i]));
    result = CGAL::minkowski_sum_3(result, next);
  }
  return result;
}

}  // namespace

Nef_polyhedron_3 ConstructRegion3(const hypo::Region3& x) {
  if (auto obj_ptr =
          std::get_if<std::shared_ptr<const hypo::TriangleList3>>(&x)) {
    return ConstructRegion3(**obj_ptr);
  } else if (auto obj_ptr =
                 std::get_if<std::shared_ptr<const hypo::Box3>>(&x)) {
    return ConstructRegion3(**obj_ptr);
  } else if (auto obj_ptr =
                 std::get_if<std::shared_ptr<const hypo::Extrude>>(&x)) {
    return ConstructRegion3(**obj_ptr);
  } else if (auto obj_ptr =
                 std::get_if<std::shared_ptr<const hypo::Transform3>>(&x)) {
    return ConstructRegion3(**obj_ptr);
  } else if (auto obj_ptr =
                 std::get_if<std::shared_ptr<const hypo::Union3>>(&x)) {
    return ConstructRegion3(**obj_ptr);
  } else if (auto obj_ptr =
                 std::get_if<std::shared_ptr<const hypo::Intersection3>>(&x)) {
    return ConstructRegion3(**obj_ptr);
  } else if (auto obj_ptr =
                 std::get_if<std::shared_ptr<const hypo::Difference3>>(&x)) {
    return ConstructRegion3(**obj_ptr);
  } else if (auto obj_ptr =
                 std::get_if<std::shared_ptr<const hypo::Icosahedron>>(&x)) {
    return ConstructRegion3(**obj_ptr);
  } else if (auto obj_ptr =
                 std::get_if<std::shared_ptr<const hypo::Octahedron>>(&x)) {
    return ConstructRegion3(**obj_ptr);
  } else if (auto obj_ptr =
                 std::get_if<std::shared_ptr<const hypo::Subdivide>>(&x)) {
    return ConstructRegion3(**obj_ptr);
  } else if (auto obj_ptr =
                 std::get_if<std::shared_ptr<const hypo::SubdivideSphere>>(
                     &x)) {
    return ConstructRegion3(**obj_ptr);
  } else if (auto obj_ptr =
                 std::get_if<std::shared_ptr<const hypo::MinkowskiSum3>>(&x)) {
    return ConstructRegion3(**obj_ptr);
  }

  std::cerr << "Unhandled Region3 type." << std::endl;
  assert(false);
  return Nef_polyhedron_3();
}

}  // namespace cgal
}  // namespace hypo