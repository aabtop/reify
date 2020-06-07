#include "cgal/subdivide.h"

#include <CGAL/boost/graph/convert_nef_polyhedron_to_polygon_mesh.h>
#include <CGAL/subdivision_method_3.h>

#include "cgal/types_nef_polyhedron_3.h"
#include "cgal/types_surface_mesh.h"

namespace hypo {
namespace cgal {

Nef_polyhedron_3 Subdivide(const Nef_polyhedron_3& src,
                           hypo::SubdivideMethod method, int iterations) {
  Surface_mesh work_mesh;
  CGAL::convert_nef_polyhedron_to_polygon_mesh(src, work_mesh);

  switch (method) {
    case hypo::SubdivideMethod::Loop: {
      CGAL::Subdivision_method_3::Loop_subdivision(
          work_mesh, CGAL::parameters::number_of_iterations(iterations));
    } break;
    case hypo::SubdivideMethod::Sqrt3: {
      CGAL::Subdivision_method_3::Sqrt3_subdivision(
          work_mesh, CGAL::parameters::number_of_iterations(iterations));
    } break;
    default: {
      assert(false);
    } break;
  }

  return Nef_polyhedron_3(work_mesh);
}

}  // namespace cgal
}  // namespace hypo
