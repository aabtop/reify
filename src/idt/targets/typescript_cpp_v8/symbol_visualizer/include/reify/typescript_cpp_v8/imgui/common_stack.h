#ifndef _IDT_TARGETS_TYPESCRIPT_CPP_V8_SYMBOL_VISUALIZER_IMGUI_COMMON_STACK_H_
#define _IDT_TARGETS_TYPESCRIPT_CPP_V8_SYMBOL_VISUALIZER_IMGUI_COMMON_STACK_H_

#include <algorithm>
#include <vector>

#include "reify/typescript_cpp_v8/imgui/docking_freespace_to_window_viewport_layer.h"
#include "reify/typescript_cpp_v8/imgui/docking_layer.h"
#include "reify/typescript_cpp_v8/imgui/layer_stack.h"
#include "reify/typescript_cpp_v8/imgui/project_layer.h"
#include "reify/typescript_cpp_v8/imgui/runtime_layer.h"
#include "reify/typescript_cpp_v8/imgui/status_layer.h"

namespace reify {
namespace typescript_cpp_v8 {
namespace imgui {

// A common set of imgui layers that are useful in almost any kind of
// visualization context. Ultimately this provides default construction for
// a bunch of components that are generally useful, and is intended to then be
// used as an input for MergeCommonAndCustomLayers to create a LayerStack
// object.
struct CommonLayers {
  CommonLayers(
      const std::function<void(std::function<void()>)>& enqueue_task_function,
      SymbolVisualizer* symbol_visualizer)
      : symbol_visualizer_viewport(symbol_visualizer),
        docking_layer(ImGuiDir_Right, 0.2f),
        docking_freespace_to_window_viewport_layer(&symbol_visualizer_viewport,
                                                   &docking_layer),
        status_layer(&docking_layer),
        runtime_layer(enqueue_task_function, &docking_layer, &status_layer,
                      symbol_visualizer) {}

  window::WindowViewport symbol_visualizer_viewport;
  DockingLayer docking_layer;
  DockingFreespaceToWindowViewportLayer
      docking_freespace_to_window_viewport_layer;
  StatusLayer status_layer;
  RuntimeLayer runtime_layer;
};

LayerStack MergeCommonAndCustomLayers(
    CommonLayers* common_layers,
    const std::vector<LayerStack::Layer>& custom_layers = {});

}  // namespace imgui
}  // namespace typescript_cpp_v8
}  // namespace reify

#endif  // _IDT_TARGETS_TYPESCRIPT_CPP_V8_SYMBOL_VISUALIZER_IMGUI_COMMON_STACK_H_
