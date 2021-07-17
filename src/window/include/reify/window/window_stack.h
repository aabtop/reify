#ifndef _REIFY_WINDOW_WINDOW_STACK_H
#define _REIFY_WINDOW_WINDOW_STACK_H

#include <memory>
#include <vector>

#include "reify/window/window.h"

namespace reify {
namespace window {

class WindowStack : public Window {
 public:
  WindowStack(const std::vector<Window*>& sub_windows);

  bool OnInputEvent(const InputEvent& input_event) override;

  void OnViewportResize(const std::array<int, 2>& size) override;

  void AdvanceTime(std::chrono::duration<float> seconds) override;

  utils::ErrorOr<std::unique_ptr<Renderer>> CreateRenderer(
      VkInstance instance, VkPhysicalDevice physical_device, VkDevice device,
      VkFormat output_image_format) override;

 private:
  std::vector<Window*> sub_windows_;
};

}  // namespace window
}  // namespace reify

#endif  // _REIFY_WINDOW_WINDOW_STACK_H
