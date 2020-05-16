#include "cgal/construct_region3.h"

#include <CGAL/Nef_polyhedron_3.h>

#include "cgal/embed_2d_in_3d.h"
#include "cgal/types.h"
#include "construct_region2.h"
#include "hypo.h"

namespace hypo {
namespace cgal {

namespace {

Point_3 ToPoint3(const hypo::Vec3& vec3) {
  return Point_3(vec3[0], vec3[1], vec3[2]);
}

Nef_polyhedron_3 ConstructRegion3(const hypo::Extrude& x) {
  return EmbedPolygonSetAs3DSurfaceMesh(ConstructRegion2(x.source));
}

}  // namespace

Nef_polyhedron_3 ConstructRegion3(const hypo::Region3& x) {
  if (auto obj_ptr = std::get_if<std::shared_ptr<hypo::Extrude>>(&x)) {
    return ConstructRegion3(**obj_ptr);
  }

  std::cerr << "Unhandled Region3 type." << std::endl;
  assert(false);
  return Nef_polyhedron_3();
}

}  // namespace cgal
}  // namespace hypo