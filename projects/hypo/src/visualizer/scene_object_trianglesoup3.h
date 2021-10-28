#ifndef _HYPO_VISUALIZER_OBJECT_VISUALIZER_TRIANGLESOUP3_H
#define _HYPO_VISUALIZER_OBJECT_VISUALIZER_TRIANGLESOUP3_H

#include <memory>
#include <string>

#include "hypo/geometry/triangle_soup.h"
#include "reify/pure_cpp/scene_visualizer.h"
#include "reify/pure_cpp/thread_pool_cache_runner.h"
#include "reify/purecpp/hypo.h"

namespace ImGui {
// We forward declare this class to avoid including `imfilebrowser.h`, which
// includes <windows.h>, which is huge.
class FileBrowser;
}  // namespace ImGui

namespace hypo {
namespace visualizer {

reify::utils::ErrorOr<std::shared_ptr<reify::pure_cpp::SceneObject<glm::mat4>>>
CreateSceneObjectTriangleSoup3(
    reify::pure_cpp::ThreadPoolCacheRunner* runner,
    const hypo::TriangleSoup3& data,
    const std::string& window_panel_title = std::string());

reify::utils::ErrorOr<std::shared_ptr<reify::pure_cpp::SceneObject<glm::mat4>>>
CreateSceneObjectTriangleSoupSet3(
    reify::pure_cpp::ThreadPoolCacheRunner* runner,
    const hypo::TriangleSoupSet3& data,
    const std::string& window_panel_title = std::string());

}  // namespace visualizer
}  // namespace hypo

#endif  // _HYPO_VISUALIZER_OBJECT_VISUALIZER_TRIANGLESOUP3_H
