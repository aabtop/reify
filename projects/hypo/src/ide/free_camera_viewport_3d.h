#ifndef _IDE_FREE_CAMERA_VIEWPORT_3D
#define _IDE_FREE_CAMERA_VIEWPORT_3D

#include <glm/glm.hpp>

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
  void AccumulateMouseButtonEvent(MouseButton button, bool pressed);
  // Keycode is defined by Qt's key mapping:
  // https://doc.qt.io/qt-5/qt.html#Key-enum
  void AccumulateKeyboardEvent(int key, bool pressed);

  const glm::mat4& view_matrix() const { return view_; }

 private:
  glm::mat4 view_;

  int viewport_width_in_pixels_;
  int viewport_height_in_pixels_;
};

#endif  // _IDE_FREE_CAMERA_VIEWPORT_3D
