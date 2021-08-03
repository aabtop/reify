#ifndef _REIFY_PURE_CPP_SCENE_VISUALIZER_CAMERA_3D_ARCBALL_H_
#define _REIFY_PURE_CPP_SCENE_VISUALIZER_CAMERA_3D_ARCBALL_H_

#include <chrono>
#include <glm/glm.hpp>
#include <glm/gtc/quaternion.hpp>
#include <optional>
#include <unordered_set>

#include "reify/pure_cpp/scene_visualizer_camera.h"
#include "reify/window/window.h"

namespace reify {
namespace pure_cpp {

class SceneVisualizerCamera3dArcball : public SceneVisualizerCamera<glm::mat4> {
 public:
  SceneVisualizerCamera3dArcball(int viewport_width_in_pixels,
                                 int viewport_height_in_pixels);

  void AccumulateViewportResize(int viewport_width_in_pixels,
                                int viewport_height_in_pixels) override;
  void AccumulateMouseMove(int x, int y) override;
  void AccumulateMouseButtonEvent(MouseButton button, bool pressed, int x,
                                  int y) override;
  // Keycode is defined by Qt's key mapping:
  // https://doc.qt.io/qt-5/qt.html#Key-enum
  void AccumulateKeyboardEvent(int key, bool pressed) override;

  void AccumulateMouseWheelEvent(float angle_in_degrees, int x, int y) override;

  void AccumulateTimeDelta(std::chrono::duration<float> seconds) override;

  glm::mat4 ProjectionViewMatrix(int viewport_width_in_pixels,
                                 int viewport_height_in_pixels) const override;

  // Reset all view parameters (e.g. camera orientation, position, etc..)
  void Reset() override;

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

}  // namespace pure_cpp
}  // namespace reify

#endif  // _REIFY_PURE_CPP_SCENE_VISUALIZER_CAMERA_3D_ARCBALL_H_
