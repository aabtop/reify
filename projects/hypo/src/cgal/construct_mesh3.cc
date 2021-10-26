#include "cgal/construct_mesh3.h"

#include <CGAL/boost/graph/convert_nef_polyhedron_to_polygon_mesh.h>

#include "cgal/construct_region3.h"
#include "cgal/conversions.h"
#include "cgal/types_nef_polyhedron_3.h"
#include "cgal/types_surface_mesh.h"
#include "reify/pure_cpp/cache.h"
#include "reify/purecpp/hypo.h"

namespace reify {
namespace pure_cpp {

template <>
int64_t EstimatedMemoryUsageInBytes(const ::hypo::cgal::Surface_mesh& x) {
  // Empirically discovered by recording before/after memory usage, but probably
  // wrong.
  int64_t estimated_memory_usage =
      110851 * (x.number_of_vertices() + x.number_of_removed_vertices());

  assert(estimated_memory_usage >= 0);
  return estimated_memory_usage;
}

}  // namespace pure_cpp
}  // namespace reify

namespace hypo {
namespace cgal {

namespace internal {
namespace {

Surface_mesh ConstructMesh3(reify::pure_cpp::ThreadPoolCacheRunner* runner,
                            const hypo::ClosedMesh3& x) {
  return *hypo::cgal::ConstructClosedMesh3(runner, x).Get();
}

Surface_mesh ConstructMesh3(reify::pure_cpp::ThreadPoolCacheRunner* runner,
                            const hypo::MeshFromRegion3& x) {
  auto region(ConstructRegion3(runner, x.region));

  Surface_mesh surface_mesh_result;
  CGAL::convert_nef_polyhedron_to_polygon_mesh(*region.Get(),
                                               surface_mesh_result);

  return surface_mesh_result;
}

}  // namespace
}  // namespace internal

FutureMesh3 ConstructMesh3(reify::pure_cpp::ThreadPoolCacheRunner* runner,
                           const hypo::Mesh3& x) {
  return std::visit(
      [runner](const auto& y) {
        return runner->MakeFuture<Surface_mesh>(&internal::ConstructMesh3, y);
      },
      static_cast<const hypo::Mesh3::AsVariant&>(x));
}

FutureMesh3 ConstructClosedMesh3(reify::pure_cpp::ThreadPoolCacheRunner* runner,
                                 const hypo::ClosedMesh3& x) {
  return std::visit(
      [runner](const auto& y) {
        return runner->MakeFuture<Surface_mesh>(&internal::ConstructMesh3, y);
      },
      static_cast<const hypo::ClosedMesh3::AsVariant&>(x));
}

}  // namespace cgal
}  // namespace hypo