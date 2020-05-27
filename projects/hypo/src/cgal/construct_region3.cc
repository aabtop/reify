#include "cgal/construct_region3.h"

#include <CGAL/aff_transformation_tags.h>

#include "cgal/conversions.h"
#include "cgal/embed_2d_in_3d.h"
#include "cgal/icosahedron.h"
#include "cgal/types_nef_polyhedron_3.h"
#include "construct_region2.h"
#include "hypo.h"

namespace hypo {
namespace cgal {

namespace {
Nef_polyhedron_3 ConstructRegion3(const hypo::Region3& x) {
  return hypo::cgal::ConstructRegion3(x);
}

Point_3 ToPoint3(const hypo::Vec3& vec3) {
  return Point_3(vec3[0], vec3[1], vec3[2]);
}

Nef_polyhedron_3 ConstructRegion3(const hypo::Extrude& x) {
  return EmbedPolygonSetAs3DSurfaceMesh(ConstructRegion2(x.source),
                                        std::get<0>(x.transforms),
                                        std::get<1>(x.transforms));
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

}  // namespace

Nef_polyhedron_3 ConstructRegion3(const hypo::Region3& x) {
  if (auto obj_ptr = std::get_if<std::shared_ptr<const hypo::Extrude>>(&x)) {
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
  }

  std::cerr << "Unhandled Region3 type." << std::endl;
  assert(false);
  return Nef_polyhedron_3();
}

}  // namespace cgal
}  // namespace hypo