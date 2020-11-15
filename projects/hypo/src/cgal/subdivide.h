#ifndef _HYPO_CGAL_SUBDIVIDE_H_
#define _HYPO_CGAL_SUBDIVIDE_H_

#include "cgal/types_nef_polyhedron_3.h"
#include "reify/purecpp/hypo.h"

namespace hypo {
namespace cgal {

Nef_polyhedron_3 Subdivide(const Nef_polyhedron_3& src,
                           hypo::SubdivideMethod method, int iterations);

// Subdivides a mesh with the strategy of projecting all new points onto the
// sphere with the given radius.
Nef_polyhedron_3 SubdivideSphere(const Nef_polyhedron_3& src,
                                 const hypo::Sphere& sphere, int iterations);

}  // namespace cgal
}  // namespace hypo

#endif  // _HYPO_CGAL_SUBDIVIDE_H_
