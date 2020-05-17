#ifndef _HYPO_CGAL_CONSTRUCT_REGION3_H_
#define _HYPO_CGAL_CONSTRUCT_REGION3_H_

#include "cgal/types_nef_polyhedron_3.h"
#include "hypo.h"

namespace hypo {
namespace cgal {

Nef_polyhedron_3 ConstructRegion3(const hypo::Region3& region3);

}  // namespace cgal
}  // namespace hypo

#endif  // _HYPO_CGAL_CONSTRUCT_REGION3_H_
