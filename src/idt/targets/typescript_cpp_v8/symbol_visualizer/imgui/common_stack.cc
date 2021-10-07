#include "reify/typescript_cpp_v8/imgui/common_stack.h"

namespace reify {
namespace typescript_cpp_v8 {
namespace imgui {

LayerStack MergeCommonAndCustomLayers(
    CommonLayers* common_layers,
    const std::vector<LayerStack::Layer>& custom_layers) {
  std::vector<LayerStack::Layer> final_layers{
      [layer = &common_layers->docking_layer] {
        layer->ExecuteImGuiCommands();
      },
      [layer = &common_layers->status_layer] { layer->ExecuteImGuiCommands(); },
      [layer = &common_layers->runtime_layer] {
        layer->ExecuteImGuiCommands();
      },
  };

  std::copy(custom_layers.begin(), custom_layers.end(),
            std::back_inserter(final_layers));

  // We make sure this goes in at the end.
  final_layers.push_back(
      [layer = &common_layers->docking_freespace_to_window_viewport_layer] {
        layer->ExecuteImGuiCommands();
      });

  return LayerStack(final_layers);
}

}  // namespace imgui
}  // namespace typescript_cpp_v8
}  // namespace reify
