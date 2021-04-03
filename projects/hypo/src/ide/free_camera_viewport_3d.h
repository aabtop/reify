#ifndef _IDE_FREE_CAMERA_VIEWPORT_3D
#define _IDE_FREE_CAMERA_VIEWPORT_3D

#include <glm/glm.hpp>

class FreeCameraViewport3d {
 public:
  FreeCameraViewport3d(int viewport_width_in_pixels,
                       int viewport_height_in_pixels);

  void AccumulateViewportResize(int viewport_width_in_pixels,
                                int viewport_height_in_pixels);
  void AccumulateMouseMove(int x, int y);
  void AccumulateMouseButtonEvent(int button, bool pressed);
  void AccumulateKeyboardEvent(char key, bool pressed);

  const glm::mat4& view_matrix() const { return view_; }

 private:
  glm::mat4 view_;

  int viewport_width_in_pixels_;
  int viewport_height_in_pixels_;
};

#endif  // _IDE_FREE_CAMERA_VIEWPORT_3D
