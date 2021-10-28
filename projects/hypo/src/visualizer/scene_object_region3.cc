#include "src/visualizer/scene_object_region3.h"

#include "cgal/construct_trianglesoup3.h"
#include "cgal/errors.h"
#include "reify/purecpp/hypo.h"
#include "src/visualizer/scene_object_trianglesoup3.h"

namespace hypo {
namespace visualizer {

reify::utils::ErrorOr<std::shared_ptr<reify::pure_cpp::SceneObject<glm::mat4>>>
CreateSceneObjectRegion3(reify::pure_cpp::ThreadPoolCacheRunner* runner,
                         const hypo::Region3& data) {
  return CreateSceneObjectTriangleSoup3(
      runner, hypo::TriangleSoupFromRegion3({data}), "Region3");
}

}  // namespace visualizer
}  // namespace hypo
