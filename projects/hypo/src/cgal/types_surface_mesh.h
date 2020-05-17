#ifndef _HYPO_CGAL_TYPES_SURFACE_MESH_H_
#define _HYPO_CGAL_TYPES_SURFACE_MESH_H_

#include <CGAL/Surface_mesh/Surface_mesh.h>

#include "cgal/types_core.h"

namespace hypo {
namespace cgal {

using Surface_mesh = CGAL::Surface_mesh<Point_3>;

}  // namespace cgal
}  // namespace hypo

#endif  // _HYPO_CGAL_TYPES_SURFACE_MESH_H_
