#include "src/visualizer/scene_object_region2.h"

#include "src/visualizer/scene_object_svg_elements.h"

namespace hypo {
namespace visualizer {

reify::utils::ErrorOr<std::shared_ptr<reify::pure_cpp::SceneObject<glm::mat3>>>
CreateSceneObjectRegion2(reify::pure_cpp::ThreadPoolCacheRunner* runner,
                         const hypo::Region2& data) {
  return CreateSceneObjectSvgElement(
      runner,
      reify::New(hypo::SvgPathElement(hypo::SvgPathElementFromRegion2{
          data, hypo::SvgSolidColor{{0.8, 0.8, 0.8, 1.0}}})),
      "Region2");
}

}  // namespace visualizer
}  // namespace hypo
