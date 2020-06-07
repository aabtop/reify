#ifndef _HYPO_CGAL_SUBDIVIDE_H_
#define _HYPO_CGAL_SUBDIVIDE_H_

#include "cgal/types_nef_polyhedron_3.h"
#include "hypo.h"

namespace hypo {
namespace cgal {

Nef_polyhedron_3 Subdivide(const Nef_polyhedron_3& src,
                           hypo::SubdivideMethod method, int iterations);

}  // namespace cgal
}  // namespace hypo

#endif  // _HYPO_CGAL_SUBDIVIDE_H_
