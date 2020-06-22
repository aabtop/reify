#ifndef _HYPO_CGAL_PRIMITIVES_3D_H_
#define _HYPO_CGAL_PRIMITIVES_3D_H_

#include "cgal/types_surface_mesh.h"

namespace hypo {
namespace cgal {

// Returns a unit icosahedron centered at the origin.
Surface_mesh MakeUnitIcosahedronMesh();

// Returns a unit octahedron centered at the origin.
Surface_mesh MakeUnitOctahedronMesh();

// Returns a unit box with one corner at (0, 0, 0) and the other at (1, 1, 1).
Surface_mesh MakeUnitBoxMesh();

}  // namespace cgal
}  // namespace hypo

#endif  // _HYPO_CGAL_PRIMITIVES_3D_H_