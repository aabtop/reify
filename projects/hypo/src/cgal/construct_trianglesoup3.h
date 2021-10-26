#ifndef _HYPO_CGAL_CONSTRUCT_TRIANGLE_SOUP3_H_
#define _HYPO_CGAL_CONSTRUCT_TRIANGLE_SOUP3_H_

#include "hypo/geometry/triangle_soup.h"
#include "reify/pure_cpp/thread_pool_cache_runner.h"
#include "reify/purecpp/hypo.h"

namespace hypo {
namespace cgal {

using FutureTriangleSoup3 = reify::pure_cpp::ThreadPoolCacheRunner::Future<
    std::shared_ptr<const hypo::geometry::TriangleSoup>>;

FutureTriangleSoup3 ConstructTriangleSoup3(
    reify::pure_cpp::ThreadPoolCacheRunner* runner,
    const hypo::TriangleSoup3& triangle_soup3);

using FutureTriangleSoupSet3 = reify::pure_cpp::ThreadPoolCacheRunner::Future<
    std::shared_ptr<const hypo::geometry::TriangleSoupSet>>;

FutureTriangleSoupSet3 ConstructTriangleSoupSet3(
    reify::pure_cpp::ThreadPoolCacheRunner* runner,
    const hypo::TriangleSoupSet3& triangle_soup_set3);

}  // namespace cgal
}  // namespace hypo

#endif  // _HYPO_CGAL_CONSTRUCT_TRIANGLE_SOUP3_H_
