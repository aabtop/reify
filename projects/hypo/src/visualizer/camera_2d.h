#ifndef _HYPO_VISUALIZER_CAMERA_2D_H_
#define _HYPO_VISUALIZER_CAMERA_2D_H_

#include <glm/glm.hpp>
#include <optional>

#include "reify/window/window.h"

namespace hypo {

class Camera2d {
 public:
  using MouseButton = reify::window::Window::MouseButton;

  Camera2d(int viewport_width_in_pixels, int viewport_height_in_pixels);

  void AccumulateViewportResize(int viewport_width_in_pixels,
                                int viewport_height_in_pixels);
  void AccumulateMouseMove(int x, int y);
  void AccumulateMouseButtonEvent(MouseButton button, bool pressed, int x,
                                  int y);

  void AccumulateMouseWheelEvent(float angle_in_degrees, int x, int y);

  glm::mat4 ProjectionViewMatrix(int viewport_width_in_pixels,
                                 int viewport_height_in_pixels) const;

  // Reset all view parameters (e.g. camera orientation, position, etc..)
  void Reset();

 private:
  struct Rect {
    float left;
    float top;
    float right;
    float bottom;
    float width() const { return right - left; }
    float height() const { return top - bottom; }
  };
  Rect VirtualViewport(int width, int height) const;
  glm::vec2 ViewportToVirtualPoint(const glm::vec2& viewport_point, int width,
                                   int height) const;

  // Converts window coordinates from pixel units into units of a fraction
  // of the viewport with the center as the origin.
  glm::vec2 ToViewportPoint(int x, int y) const;

  int viewport_width_in_pixels_;
  int viewport_height_in_pixels_;

  // The optional is valid whenever the mouse button is pressed.
  std::optional<glm::vec2> previous_viewport_point_;

  bool mouse_button_pressed_[static_cast<int>(MouseButton::Count)];

  // The camera's transform parameters.
  glm::vec2 center_;
  float scale_;
};

}  // namespace hypo

#endif  // _HYPO_VISUALIZER_CAMERA_2D_H_
