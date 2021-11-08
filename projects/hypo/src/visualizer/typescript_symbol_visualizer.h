#ifndef _HYPO_VISUALIZER_TYPESCRIPT_SYMBOL_VISUALIZER_H
#define _HYPO_VISUALIZER_TYPESCRIPT_SYMBOL_VISUALIZER_H

#include <thread>

#include "reify/pure_cpp/scene_visualizer_camera_2d.h"
#include "reify/pure_cpp/scene_visualizer_camera_3d_arcball.h"
#include "reify/pure_cpp/thread_pool_cache_runner.h"
#include "reify/typescript_cpp_v8/hypo.h"
#include "reify/typescript_cpp_v8/symbol_visualizer.h"
#include "src/visualizer/scene_object_boundary2.h"
#include "src/visualizer/scene_object_mesh3.h"
#include "src/visualizer/scene_object_region2.h"
#include "src/visualizer/scene_object_region3.h"
#include "src/visualizer/scene_object_svg_elements.h"
#include "src/visualizer/scene_object_trianglesoup3.h"

namespace hypo {
namespace visualizer {

class TypeScriptSymbolVisualizer {
 public:
  TypeScriptSymbolVisualizer()
      : camera_2d(0, 0),
        camera_3d(0, 0),
        runner(std::make_unique<ebb::ThreadPool>(
            std::thread::hardware_concurrency(), 256 * 1024,
            ebb::ThreadPool::SchedulingPolicy::LIFO)),
        boundary_2_visualizer(&camera_2d,
                              [runner = &this->runner](const auto& x) {
                                return CreateSceneObjectBoundary2(runner, x);
                              }),
        mesh_3_visualizer(&camera_3d,
                          [runner = &this->runner](const auto& x) {
                            return CreateSceneObjectMesh3(runner, x);
                          }),
        region_2_visualizer(&camera_2d,
                            [runner = &this->runner](const auto& x) {
                              return CreateSceneObjectRegion2(runner, x);
                            }),
        svg_elements_visualizer(&camera_2d,
                                [runner = &this->runner](const auto& x) {
                                  return CreateSceneObjectSvgElements(runner,
                                                                      x);
                                }),
        svg_element_visualizer(&camera_2d,
                               [runner = &this->runner](const auto& x) {
                                 return CreateSceneObjectSvgElement(runner, x);
                               }),
        region_3_visualizer(&camera_3d,
                            [runner = &this->runner](const auto& x) {
                              return CreateSceneObjectRegion3(runner, x);
                            }),
        triangle_soup_3_visualizer(&camera_3d,
                                   [runner = &this->runner](const auto& x) {
                                     return CreateSceneObjectTriangleSoup3(
                                         runner, x);
                                   }),
        triangle_soup_set_3_visualizer(
            &camera_3d,
            [runner = &this->runner](const auto& x) {
              return CreateSceneObjectTriangleSoupSet3(runner, x);
            }),
        visualizer(
            reify::typescript_cpp_v8::hypo::typescript_declarations(),
            {
                *reify::typescript_cpp_v8::MakeTypeScriptSymbolVisualizer(
                    &boundary_2_visualizer),
                *reify::typescript_cpp_v8::MakeTypeScriptSymbolVisualizer(
                    &mesh_3_visualizer),
                *reify::typescript_cpp_v8::MakeTypeScriptSymbolVisualizer(
                    &region_2_visualizer),
                *reify::typescript_cpp_v8::MakeTypeScriptSymbolVisualizer(
                    &svg_elements_visualizer),
                *reify::typescript_cpp_v8::MakeTypeScriptSymbolVisualizer(
                    &svg_element_visualizer),
                *reify::typescript_cpp_v8::MakeTypeScriptSymbolVisualizer(
                    &region_3_visualizer),
                *reify::typescript_cpp_v8::MakeTypeScriptSymbolVisualizer(
                    &triangle_soup_3_visualizer),
                *reify::typescript_cpp_v8::MakeTypeScriptSymbolVisualizer(
                    &triangle_soup_set_3_visualizer),
            }) {}

  reify::pure_cpp::SceneVisualizerCamera2d camera_2d;
  reify::pure_cpp::SceneVisualizerCamera3dArcball camera_3d;

  reify::pure_cpp::ThreadPoolCacheRunner runner;

  reify::pure_cpp::SceneVisualizer<hypo::Boundary2, glm::mat3>
      boundary_2_visualizer;
  reify::pure_cpp::SceneVisualizer<hypo::Mesh3, glm::mat4> mesh_3_visualizer;
  reify::pure_cpp::SceneVisualizer<hypo::Region2, glm::mat3>
      region_2_visualizer;
  reify::pure_cpp::SceneVisualizer<hypo::SvgElements, glm::mat3>
      svg_elements_visualizer;
  reify::pure_cpp::SceneVisualizer<hypo::SvgElement, glm::mat3>
      svg_element_visualizer;
  reify::pure_cpp::SceneVisualizer<hypo::Region3, glm::mat4>
      region_3_visualizer;
  reify::pure_cpp::SceneVisualizer<hypo::TriangleSoup3, glm::mat4>
      triangle_soup_3_visualizer;
  reify::pure_cpp::SceneVisualizer<hypo::TriangleSoupSet3, glm::mat4>
      triangle_soup_set_3_visualizer;

  reify::typescript_cpp_v8::SymbolVisualizer visualizer;
};

}  // namespace visualizer
}  // namespace hypo

#endif  // _HYPO_VISUALIZER_TYPESCRIPT_SYMBOL_VISUALIZER_H
