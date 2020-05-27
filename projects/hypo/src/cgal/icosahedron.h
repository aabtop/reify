#ifndef _HYPO_CGAL_ICOSAHEDRON_H_
#define _HYPO_CGAL_ICOSAHEDRON_H_

#include "cgal/types_surface_mesh.h"

namespace hypo {
namespace cgal {

// Returns a unit icosahedron centered at the origin.
Surface_mesh MakeUnitIcosahedronMesh();

}  // namespace cgal
}  // namespace hypo

#endif  // _HYPO_CGAL_ICOSAHEDRON_H_