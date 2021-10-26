#ifndef _HYPO_VISUALIZER_OBJECT_VISUALIZER_MESH3_H
#define _HYPO_VISUALIZER_OBJECT_VISUALIZER_MESH3_H

#include <glm/glm.hpp>
#include <memory>

#include "reify/pure_cpp/scene_visualizer.h"
#include "reify/pure_cpp/thread_pool_cache_runner.h"
#include "reify/purecpp/hypo.h"

namespace hypo {
namespace visualizer {

reify::utils::ErrorOr<std::shared_ptr<reify::pure_cpp::SceneObject<glm::mat4>>>
CreateSceneObjectMesh3(reify::pure_cpp::ThreadPoolCacheRunner* runner,
                       const hypo::Mesh3& data);

}  // namespace visualizer
}  // namespace hypo

#endif  // _HYPO_VISUALIZER_OBJECT_VISUALIZER_MESH3_H
