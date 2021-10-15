#ifndef _HYPO_CGAL_SUBDIVIDE_H_
#define _HYPO_CGAL_SUBDIVIDE_H_

#include "cgal/types_surface_mesh.h"
#include "reify/purecpp/hypo.h"

namespace hypo {
namespace cgal {

Surface_mesh Subdivide(const Surface_mesh& src, hypo::SubdivideMethod method,
                       int iterations);

// Subdivides a mesh with the strategy of projecting all new points onto the
// sphere with the given radius.
Surface_mesh SubdivideSphere(const Surface_mesh& src,
                             const hypo::Sphere& sphere, int iterations);

}  // namespace cgal
}  // namespace hypo

#endif  // _HYPO_CGAL_SUBDIVIDE_H_
