#include "free_camera_viewport_3d.h"

#include <glm/gtc/matrix_transform.hpp>

FreeCameraViewport3d::FreeCameraViewport3d(int viewport_width_in_pixels,
                                           int viewport_height_in_pixels)
    : view_(glm::translate(glm::mat4(1.0f),
                           glm::vec3(0.0f, 0.0f, -8.0f))),  // view_(1.0),
      viewport_width_in_pixels_(viewport_width_in_pixels),
      viewport_height_in_pixels_(viewport_height_in_pixels) {}

void FreeCameraViewport3d::AccumulateViewportResize(
    int viewport_width_in_pixels, int viewport_height_in_pixels) {
  viewport_width_in_pixels_ = viewport_width_in_pixels;
  viewport_height_in_pixels_ = viewport_height_in_pixels;
}

void FreeCameraViewport3d::AccumulateMouseMove(int x, int y) {}
void FreeCameraViewport3d::AccumulateMouseButtonEvent(MouseButton button,
                                                      bool pressed) {}
void FreeCameraViewport3d::AccumulateKeyboardEvent(int key, bool pressed) {}
