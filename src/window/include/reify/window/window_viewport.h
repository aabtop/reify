#ifndef _REIFY_WINDOW_WINDOW_VIEWPORT_H
#define _REIFY_WINDOW_WINDOW_VIEWPORT_H

#include <memory>
#include <vector>

#include "reify/window/window.h"

namespace reify {
namespace window {

// A wrapper around an existing window which maps that window onto a section (or
// viewport) of a larger window.
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

  const std::optional<std::array<int, 2>>& get_on_viewport_resize_size() const {
    return on_viewport_resize_size_;
  }

 private:
  Window* sub_window_;

  // Size according to whatever was last set by the call to OnViewportResize().
  // This value is taken to be the viewport parent size, and will be used to
  // possibly scale the renderer viewport if it differs from the framebuffer
  // size (e.g. for high DPI displays).
  std::optional<std::array<int, 2>> on_viewport_resize_size_;

  // The viewport set via SetViewport(), which will affect coordinates passed
  // down into sub_window_.
  std::optional<Rect> viewport_;
};

}  // namespace window
}  // namespace reify

#endif  // _REIFY_WINDOW_WINDOW_VIEWPORT_H
