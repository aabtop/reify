#ifndef _HYPO_CGAL_CONSTRUCT_REGION2_H_
#define _HYPO_CGAL_CONSTRUCT_REGION2_H_

#include <memory>

#include "cgal/types.h"
#include "hypo.h"

namespace hypo {
namespace cgal {

Polygon_set_2 ConstructRegion2(const hypo::Region2& x);

}  // namespace cgal
}  // namespace hypo

#endif  // _HYPO_CGAL_CONSTRUCT_REGION2_H_
