#ifndef _HYPO_VISUALIZER_OBJECT_VISUALIZER_REGION3_H
#define _HYPO_VISUALIZER_OBJECT_VISUALIZER_REGION3_H

#include <any>
#include <memory>
#include <optional>
#include <string>

#include "cgal/types_nef_polyhedron_3.h"
#include "reify/pure_cpp/scene_visualizer.h"
#include "reify/pure_cpp/scene_visualizer_camera_3d_arcball.h"
#include "reify/pure_cpp/thread_pool_cache_runner.h"
#include "reify/purecpp/hypo.h"
#include "src/visualizer/vulkan/flat_shaded_triangle_renderer3.h"

namespace ImGui {
// We forward declare this class to avoid including `imfilebrowser.h`, which
// includes <windows.h>, which is huge.
class FileBrowser;
}  // namespace ImGui

namespace hypo {
namespace visualizer {

reify::utils::ErrorOr<std::shared_ptr<reify::pure_cpp::SceneObject<glm::mat4>>>
CreateSceneObjectRegion3(reify::pure_cpp::ThreadPoolCacheRunner* runner,
                         const hypo::Region3& data);

}  // namespace visualizer
}  // namespace hypo

#endif  // _HYPO_VISUALIZER_OBJECT_VISUALIZER_REGION3_H
