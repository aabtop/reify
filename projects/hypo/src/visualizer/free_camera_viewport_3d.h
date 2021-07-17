#ifndef _IDE_FREE_CAMERA_VIEWPORT_3D
#define _IDE_FREE_CAMERA_VIEWPORT_3D

#include <chrono>
#include <glm/glm.hpp>
#include <glm/gtc/quaternion.hpp>
#include <optional>
#include <unordered_set>

#include "reify/window/window.h"

class FreeCameraViewport3d {
 public:
  using MouseButton = reify::window::Window::MouseButton;

  FreeCameraViewport3d(int viewport_width_in_pixels,
                       int viewport_height_in_pixels);

  void AccumulateViewportResize(int viewport_width_in_pixels,
                                int viewport_height_in_pixels);
  void AccumulateMouseMove(int x, int y);
  void AccumulateMouseButtonEvent(MouseButton button, bool pressed, int x,
                                  int y);
  // Keycode is defined by Qt's key mapping:
  // https://doc.qt.io/qt-5/qt.html#Key-enum
  void AccumulateKeyboardEvent(int key, bool pressed);

  void AccumulateMouseWheelEvent(float angle_in_degrees);

  void AccumulateTimeDelta(std::chrono::duration<float> seconds);

  glm::mat4 ViewMatrix() const;

  // Reset all view parameters (e.g. camera orientation, position, etc..)
  void Reset();

 private:
  glm::vec3 ToArcballPoint(const glm::vec2& viewport_point) const;
  // Converts window coordinates from pixel units into units of a fraction
  // of the viewport with the center as the origin.
  glm::vec2 ToViewportPoint(int x, int y) const;

  int viewport_width_in_pixels_;
  int viewport_height_in_pixels_;

  // The optional is valid whenever the mouse button is pressed.
  std::optional<glm::vec2> previous_viewport_point_;

  std::unordered_set<int> keys_pressed_;

  bool mouse_button_pressed_[static_cast<int>(MouseButton::Count)];

  // The camera's transform parameters.

  // The position the arcball rotations are orbiting around.
  glm::vec3 focus_position_;

  // The distance the camera is from the `focus_position_`.  In a way this can
  // be thought of as the zoom level.
  float camera_distance_from_focus_;

  // The camera's orientation around the focus point.
  glm::quat camera_orientation_;
};

#endif  // _IDE_FREE_CAMERA_VIEWPORT_3D
