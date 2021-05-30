#ifndef _REIFY_WINDOW_WINDOW_H
#define _REIFY_WINDOW_WINDOW_H

#include <vulkan/vulkan.h>

#include <any>
#include <array>
#include <chrono>
#include <functional>
#include <optional>
#include <string>
#include <variant>
#include <vector>

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
  bool operator==(const Rect& x) const = default;
};

// An abstract Window, which accepts input and renders with Vulkan.
// The intention is that this can be wrapped by concrete window types, such
// as concrete platform windows (like Win32 and X11), or as a widget inside
// of a framework like Qt.
class Window {
 public:
  struct Error {
    std::string msg;
  };
  template <typename T>
  using ErrorOr = std::variant<Error, T>;

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
  };
  struct KeyboardEvent {
    // Keycode is defined by Qt's key mapping:
    // https://doc.qt.io/qt-5/qt.html#Key-enum
    int key;
    bool pressed;
  };
  using InputEvent = std::variant<MouseMoveEvent, MouseButtonEvent,
                                  MouseWheelEvent, KeyboardEvent>;

  class Renderer {
   public:
    template <typename T>
    using ErrorOr = ErrorOr<T>;
    using FrameResources = std::any;
    virtual ~Renderer() {}
    virtual ErrorOr<FrameResources> RenderFrame(
        VkCommandBuffer command_buffer, VkFramebuffer framebuffer,
        VkImage output_color_image, const Rect& viewport_region) = 0;
  };

  virtual ~Window(){};

  virtual bool OnInputEvent(const InputEvent& input_event) = 0;

  virtual void OnViewportResize(const std::array<int, 2>& size) = 0;

  virtual void AdvanceTime(std::chrono::duration<float> seconds) = 0;

  virtual ErrorOr<std::unique_ptr<Renderer>> CreateRenderer(
      VkInstance instance, VkPhysicalDevice physical_device, VkDevice device,
      VkFormat output_image_format) = 0;
};

}  // namespace window
}  // namespace reify

#endif  // _REIFY_WINDOW_WINDOW_H
