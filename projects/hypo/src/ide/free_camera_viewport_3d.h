#ifndef _IDE_FREE_CAMERA_VIEWPORT_3D
#define _IDE_FREE_CAMERA_VIEWPORT_3D

#include <glm/glm.hpp>
#include <glm/gtc/quaternion.hpp>
#include <optional>

class FreeCameraViewport3d {
 public:
  enum class MouseButton {
    Left,
    Unknown,
  };
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

  glm::mat4 ViewMatrix() const;

 private:
  glm::vec3 ToArcballPoint(int x, int y);

  int viewport_width_in_pixels_;
  int viewport_height_in_pixels_;

  // The optional is valid whenever the mouse button is pressed.
  std::optional<glm::vec3> previous_arcball_point_;

  // The camera's transform parameters.
  glm::vec3 focus_position_;
  float camera_distance_from_focus_;
  glm::quat camera_orientation_;
};

#endif  // _IDE_FREE_CAMERA_VIEWPORT_3D
