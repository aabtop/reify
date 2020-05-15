#include "cgal/construct_region3.h"

#include <CGAL/Nef_polyhedron_3.h>

#include "cgal/types.h"
#include "hypo.h"

namespace hypo {
namespace cgal {

std::shared_ptr<CGAL::Nef_polyhedron_3<Kernel>> ConstructRegion3(
    const hypo::Region3& region3) {
  return std::shared_ptr<CGAL::Nef_polyhedron_3<Kernel>>();
}

}  // namespace cgal
}  // namespace hypo