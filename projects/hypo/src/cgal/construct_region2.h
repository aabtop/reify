#ifndef _HYPO_CGAL_CONSTRUCT_REGION3_H_
#define _HYPO_CGAL_CONSTRUCT_REGION3_H_

#include <memory>

#include "cgal/types.h"
#include "hypo.h"

namespace hypo {
namespace cgal {

std::shared_ptr<Polygon_set_2> ConstructRegion2(const hypo::Region2& region2);

}  // namespace cgal
}  // namespace hypo

#endif  // _HYPO_CGAL_CONSTRUCT_REGION3_H_
