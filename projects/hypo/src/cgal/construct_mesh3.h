#ifndef _HYPO_CGAL_CONSTRUCT_MESH3_H_
#define _HYPO_CGAL_CONSTRUCT_MESH3_H_

#include "cgal/types_surface_mesh.h"
#include "reify/pure_cpp/thread_pool_cache_runner.h"
#include "reify/purecpp/hypo.h"

namespace hypo {
namespace cgal {

using FutureMesh3 = reify::pure_cpp::ThreadPoolCacheRunner::Future<
    std::shared_ptr<const Surface_mesh>>;

FutureMesh3 ConstructMesh3(reify::pure_cpp::ThreadPoolCacheRunner* runner,
                           const hypo::Mesh3& mesh3);

FutureMesh3 ConstructClosedMesh3(reify::pure_cpp::ThreadPoolCacheRunner* runner,
                                 const hypo::ClosedMesh3& mesh3);

}  // namespace cgal
}  // namespace hypo

#endif  // _HYPO_CGAL_CONSTRUCT_MESH3_H_
