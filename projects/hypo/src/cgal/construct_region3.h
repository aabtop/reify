#ifndef _HYPO_CGAL_CONSTRUCT_REGION3_H_
#define _HYPO_CGAL_CONSTRUCT_REGION3_H_

#include "cgal/types_nef_polyhedron_3.h"
#include "reify/pure_cpp/thread_pool_cache_runner.h"
#include "reify/purecpp/hypo.h"

namespace hypo {
namespace cgal {

using FutureRegion3 = reify::pure_cpp::ThreadPoolCacheRunner::Future<
    std::shared_ptr<const Nef_polyhedron_3>>;

FutureRegion3 ConstructRegion3(reify::pure_cpp::ThreadPoolCacheRunner* runner,
                               const hypo::Region3& region3);

}  // namespace cgal
}  // namespace hypo

#endif  // _HYPO_CGAL_CONSTRUCT_REGION3_H_
