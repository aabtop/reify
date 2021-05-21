#ifndef _IDT_TARGETS_TYPESCRIPT_CPP_V8_DOMAIN_VISUALIZER_IMGUI_PROJECT_LAYER_H
#define _IDT_TARGETS_TYPESCRIPT_CPP_V8_DOMAIN_VISUALIZER_IMGUI_PROJECT_LAYER_H

#include "reify/typescript_cpp_v8/imgui/runtime_layer.h"

namespace reify {
namespace typescript_cpp_v8 {
namespace imgui {

class ProjectLayer {
 public:
  ProjectLayer(RuntimeLayer* runtime_layer) : runtime_layer_(runtime_layer) {}

  void ExecuteImGuiCommands();

 private:
  RuntimeLayer* runtime_layer_;

  bool checkbox_ = false;
  float f_ = 0.25f;
  int counter_ = 5;
};

}  // namespace imgui
}  // namespace typescript_cpp_v8
}  // namespace reify

#endif  // _IDT_TARGETS_TYPESCRIPT_CPP_V8_DOMAIN_VISUALIZER_IMGUI_PROJECT_LAYER_H
