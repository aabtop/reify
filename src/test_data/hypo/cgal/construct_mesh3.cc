#include "cgal/construct_mesh3.h"

#include <CGAL/Nef_polyhedron_3.h>

#include "cgal/types.h"
#include "hypo.h"

namespace hypo {
namespace cgal {

std::shared_ptr<CGAL::Nef_polyhedron_3<Kernel>> ConstructMesh3(
    const hypo::Mesh3& mesh3) {
  return std::shared_ptr<CGAL::Nef_polyhedron_3<Kernel>>();
}

}  // namespace cgal
}  // namespace hypo