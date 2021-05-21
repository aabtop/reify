#ifndef _IDT_TARGETS_TYPESCRIPT_CPP_V8_DOMAIN_VISUALIZER_IMGUI_PROJECT_LAYER_H
#define _IDT_TARGETS_TYPESCRIPT_CPP_V8_DOMAIN_VISUALIZER_IMGUI_PROJECT_LAYER_H

namespace reify {
namespace typescript_cpp_v8 {
namespace imgui {

class ProjectLayer {
 public:
  ProjectLayer();

  void ExecuteImGuiCommands();

 private:
  bool checkbox_ = false;
  float f_ = 0.25f;
  int counter_ = 5;
};

}  // namespace imgui
}  // namespace typescript_cpp_v8
}  // namespace reify

#endif  // _IDT_TARGETS_TYPESCRIPT_CPP_V8_DOMAIN_VISUALIZER_IMGUI_PROJECT_LAYER_H
