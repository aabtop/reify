#ifndef _HYPO_VISUALIZER_SCENE_OBJECT_BOUNDARY2_H
#define _HYPO_VISUALIZER_SCENE_OBJECT_BOUNDARY2_H

#include <glm/glm.hpp>
#include <memory>

#include "reify/pure_cpp/scene_visualizer.h"
#include "reify/pure_cpp/thread_pool_cache_runner.h"
#include "reify/purecpp/hypo.h"
#include "reify/utils/error.h"

namespace hypo {
namespace visualizer {

reify::utils::ErrorOr<std::shared_ptr<reify::pure_cpp::SceneObject<glm::mat3>>>
CreateSceneObjectBoundary2(reify::pure_cpp::ThreadPoolCacheRunner* runner,
                           const hypo::Boundary2& data);

}  // namespace visualizer
}  // namespace hypo

#endif  // _HYPO_VISUALIZER_SCENE_OBJECT_BOUNDARY2_H
