#ifndef _IDT_TARGETS_TYPESCRIPT_CPP_V8_SYMBOL_VISUALIZER_IMGUI_DOCKING_FREESPACE_TO_WINDOW_VIEWPORT_LAYER_H
#define _IDT_TARGETS_TYPESCRIPT_CPP_V8_SYMBOL_VISUALIZER_IMGUI_DOCKING_FREESPACE_TO_WINDOW_VIEWPORT_LAYER_H

#include "imgui.h"
#include "reify/typescript_cpp_v8/imgui/docking_layer.h"
#include "reify/window/window_viewport.h"

namespace reify {
namespace typescript_cpp_v8 {
namespace imgui {

class DockingFreespaceToWindowViewportLayer {
 public:
  DockingFreespaceToWindowViewportLayer(window::WindowViewport* window_viewport,
                                        DockingLayer* docking_layer)
      : window_viewport_(window_viewport), docking_layer_(docking_layer){};

  void ExecuteImGuiCommands();

 private:
  window::WindowViewport* window_viewport_;
  DockingLayer* docking_layer_;
};

}  // namespace imgui
}  // namespace typescript_cpp_v8
}  // namespace reify

#endif  // _IDT_TARGETS_TYPESCRIPT_CPP_V8_SYMBOL_VISUALIZER_IMGUI_DOCKING_FREESPACE_TO_WINDOW_VIEWPORT_LAYER_H
