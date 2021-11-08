#include "src/visualizer/scene_object_region2.h"
#include "src/visualizer/scene_object_svg_elements.h"

namespace hypo {
namespace visualizer {

reify::utils::ErrorOr<std::shared_ptr<reify::pure_cpp::SceneObject<glm::mat3>>>
CreateSceneObjectBoundary2(reify::pure_cpp::ThreadPoolCacheRunner* runner,
                           const hypo::Boundary2& data) {
  return CreateSceneObjectSvgElement(
      runner,
      hypo::SvgElement(
          reify::New(hypo::SvgPathElement(hypo::SvgPathElementFromBoundary2{
              data, hypo::SvgSolidColor{{0.95, 0.95, 0.95, 1.0}},
              hypo::SvgPercentage{1.0f}}))),
      "Boundary2");
}

}  // namespace visualizer
}  // namespace hypo
