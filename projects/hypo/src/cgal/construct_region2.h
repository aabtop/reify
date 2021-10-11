#ifndef _HYPO_CGAL_CONSTRUCT_REGION2_H_
#define _HYPO_CGAL_CONSTRUCT_REGION2_H_

#include <memory>

#include "cgal/types_polygons.h"
#include "reify/pure_cpp/thread_pool_cache_runner.h"
#include "reify/purecpp/hypo.h"

namespace hypo {
namespace cgal {

reify::pure_cpp::ThreadPoolCacheRunner::Future<
    std::shared_ptr<const Polygon_set_2>>
ConstructRegion2(reify::pure_cpp::ThreadPoolCacheRunner* runner,
                 const hypo::Region2& x);

reify::pure_cpp::ThreadPoolCacheRunner::Future<
    std::shared_ptr<const Polygon_set_2>>
ConstructBoundary2(reify::pure_cpp::ThreadPoolCacheRunner* runner,
                   const hypo::Boundary2& x);

}  // namespace cgal
}  // namespace hypo

#endif  // _HYPO_CGAL_CONSTRUCT_REGION2_H_
