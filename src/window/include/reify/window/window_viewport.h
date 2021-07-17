#ifndef _REIFY_WINDOW_WINDOW_VIEWPORT_H
#define _REIFY_WINDOW_WINDOW_VIEWPORT_H

#include <memory>
#include <vector>

#include "reify/window/window.h"

namespace reify {
namespace window {

class WindowViewport : public Window {
 public:
  WindowViewport(Window* sub_window);

  bool OnInputEvent(const InputEvent& input_event) override;

  void OnViewportResize(const std::array<int, 2>& size) override;

  void AdvanceTime(std::chrono::duration<float> seconds) override;

  utils::ErrorOr<std::unique_ptr<Renderer>> CreateRenderer(
      VkInstance instance, VkPhysicalDevice physical_device, VkDevice device,
      VkFormat output_image_format) override;

  void SetViewport(const Rect& viewport);
  const std::optional<Rect>& viewport() const { return viewport_; }

 private:
  Window* sub_window_;

  std::optional<Rect> viewport_;
};

}  // namespace window
}  // namespace reify

#endif  // _REIFY_WINDOW_WINDOW_VIEWPORT_H
