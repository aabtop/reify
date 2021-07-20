#ifndef _REIFY_WINDOW_WINDOW_H
#define _REIFY_WINDOW_WINDOW_H

#include <vulkan/vulkan.h>

#include <any>
#include <array>
#include <chrono>
#include <functional>
#include <memory>
#include <optional>
#include <string>
#include <variant>
#include <vector>

#include "reify/utils/error.h"

namespace reify {
namespace window {

struct Rect {
  int left;
  int top;
  int right;
  int bottom;

  int width() const { return right - left; }
  int height() const { return bottom - top; }
  bool inside(int x, int y) const {
    return left <= x && x < right && top <= y && y < bottom;
  }
  bool operator==(const Rect& x) const {
    return left == x.left && top == x.top && right == x.right &&
           bottom == x.bottom;
  }

  static Rect Intersect(const Rect& a, const Rect& b) {
    return Rect{
        std::max(a.left, b.left),
        std::max(a.top, b.top),
        std::min(a.right, b.right),
        std::min(a.bottom, b.bottom),
    };
  }
};

// An abstract Window, which accepts input and renders with Vulkan.
// The intention is that this can be wrapped by concrete window types, such
// as concrete platform windows (like Win32 and X11), or as a widget inside
// of a framework like Qt.
class Window {
 public:
  struct MouseMoveEvent {
    int x;
    int y;
  };
  enum class MouseButton {
    Left,
    Right,
    Unknown,
    Count = Unknown,
  };
  struct MouseButtonEvent {
    MouseButton button;
    bool pressed;
    int x;
    int y;
  };
  struct MouseWheelEvent {
    float angle_in_degrees;
    int x;
    int y;
  };
  struct KeyboardEvent {
    // Key codes for key events, adopted from Windows virtual key codes:
    //   https://msdn.microsoft.com/en-us/library/windows/desktop/dd375731%28v=vs.85%29.aspx
    int key;
    bool pressed;
  };
  using InputEvent = std::variant<MouseMoveEvent, MouseButtonEvent,
                                  MouseWheelEvent, KeyboardEvent>;

  class Renderer {
   public:
    using FrameResources = std::any;
    virtual ~Renderer() {}
    virtual utils::ErrorOr<FrameResources> RenderFrame(
        VkCommandBuffer command_buffer, VkFramebuffer framebuffer,
        VkImage output_color_image, const Rect& viewport_region) = 0;
  };

  virtual ~Window(){};

  virtual bool OnInputEvent(const InputEvent& input_event) = 0;

  virtual void OnViewportResize(const std::array<int, 2>& size) = 0;

  virtual void AdvanceTime(std::chrono::duration<float> seconds) = 0;

  virtual utils::ErrorOr<std::unique_ptr<Renderer>> CreateRenderer(
      VkInstance instance, VkPhysicalDevice physical_device, VkDevice device,
      VkFormat output_image_format) = 0;
};

}  // namespace window
}  // namespace reify

#endif  // _REIFY_WINDOW_WINDOW_H
