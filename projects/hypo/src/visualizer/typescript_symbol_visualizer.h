#ifndef _HYPO_VISUALIZER_VISUALIZER_H
#define _HYPO_VISUALIZER_VISUALIZER_H

#include "reify/pure_cpp/scene_visualizer_camera_2d.h"
#include "reify/pure_cpp/scene_visualizer_camera_3d_arcball.h"
#include "reify/typescript_cpp_v8/hypo.h"
#include "reify/typescript_cpp_v8/symbol_visualizer.h"
#include "src/visualizer/object_visualizer_region2.h"
#include "src/visualizer/object_visualizer_region3.h"

namespace hypo {

class TypeScriptSymbolVisualizer {
 public:
  TypeScriptSymbolVisualizer()
      : camera_2d(0, 0),
        camera_3d(0, 0),
        region_2_visualizer(&camera_2d, &CreateSceneObjectRegion2),
        region_3_visualizer(&camera_3d, &CreateSceneObjectRegion3),
        // region_3_visualizer(&camera_3d, &scene_object_class_region_3),
        visualizer(
            reify::typescript_cpp_v8::hypo::typescript_declarations(),
            {
                *reify::typescript_cpp_v8::MakeTypeScriptSymbolVisualizer(
                    &region_2_visualizer),
                *reify::typescript_cpp_v8::MakeTypeScriptSymbolVisualizer(
                    &region_3_visualizer),
            }) {}

  reify::pure_cpp::SceneVisualizerCamera2d camera_2d;
  reify::pure_cpp::SceneVisualizerCamera3dArcball camera_3d;

  reify::pure_cpp::SceneVisualizer<hypo::Region2, glm::mat4>
      region_2_visualizer;
  reify::pure_cpp::SceneVisualizer<hypo::Region3, glm::mat4>
      region_3_visualizer;
  // ObjectVisualizerRegion3 region_3_visualizer;

  reify::typescript_cpp_v8::SymbolVisualizer visualizer;
};

}  // namespace hypo

#endif  // _HYPO_VISUALIZER_VISUALIZER_H
