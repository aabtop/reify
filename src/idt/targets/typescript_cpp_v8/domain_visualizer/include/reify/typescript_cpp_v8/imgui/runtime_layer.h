#ifndef _IDT_TARGETS_TYPESCRIPT_CPP_V8_DOMAIN_VISUALIZER_IMGUI_RUNTIME_LAYER_H
#define _IDT_TARGETS_TYPESCRIPT_CPP_V8_DOMAIN_VISUALIZER_IMGUI_RUNTIME_LAYER_H

#include "reify/typescript_cpp_v8/domain_visualizer.h"

namespace reify {
namespace typescript_cpp_v8 {
namespace imgui {

class RuntimeLayer {
 public:
  RuntimeLayer(DomainVisualizer* domain_visualizer)
      : domain_visualizer_(domain_visualizer){};

  void ExecuteImGuiCommands();

 private:
  DomainVisualizer* domain_visualizer_;
};

}  // namespace imgui
}  // namespace typescript_cpp_v8
}  // namespace reify

#endif  // _IDT_TARGETS_TYPESCRIPT_CPP_V8_DOMAIN_VISUALIZER_IMGUI_RUNTIME_LAYER_H
