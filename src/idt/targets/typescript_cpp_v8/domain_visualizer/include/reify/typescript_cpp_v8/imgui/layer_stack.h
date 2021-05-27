#ifndef _IDT_TARGETS_TYPESCRIPT_CPP_V8_DOMAIN_VISUALIZER_IMGUI_LAYER_STACK_H
#define _IDT_TARGETS_TYPESCRIPT_CPP_V8_DOMAIN_VISUALIZER_IMGUI_LAYER_STACK_H

#include "reify/window/window.h"

namespace reify {
namespace typescript_cpp_v8 {
namespace imgui {

class LayerStack : public window::Window {
 public:
  // Called during each rendered frame to enable code to apply ImGui elements.
  using Layer = std::function<void()>;

  LayerStack(const std::vector<Layer>& layers);
  ~LayerStack();

  bool OnInputEvent(const InputEvent& input_event) override;

  void OnViewportResize(const std::array<int, 2>& size) override;

  void AdvanceTime(std::chrono::duration<float> seconds) override;

  ErrorOr<std::unique_ptr<Renderer>> CreateRenderer(
      VkInstance instance, VkPhysicalDevice physical_device, VkDevice device,
      VkFormat output_image_format) override;

 private:
  std::vector<Layer> layers_;

  std::chrono::duration<float> accumulated_time_since_last_render_ =
      std::chrono::duration<float>::zero();
};

}  // namespace imgui
}  // namespace typescript_cpp_v8
}  // namespace reify

#endif  // _IDT_TARGETS_TYPESCRIPT_CPP_V8_DOMAIN_VISUALIZER_IMGUI_LAYER_STACK_H
