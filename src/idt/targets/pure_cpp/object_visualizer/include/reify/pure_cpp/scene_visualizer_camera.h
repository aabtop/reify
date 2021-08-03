#ifndef _REIFY_PURE_CPP_SCENE_VISUALIZER_CAMERA_H_
#define _REIFY_PURE_CPP_SCENE_VISUALIZER_CAMERA_H_

#include "reify/window/window.h"

namespace reify {
namespace pure_cpp {

template <typename ViewMatrix>
class SceneVisualizerCamera {
 public:
  using MouseButton = reify::window::Window::MouseButton;

  virtual ~SceneVisualizerCamera() {}

  virtual void AccumulateViewportResize(int viewport_width_in_pixels,
                                        int viewport_height_in_pixels) = 0;
  virtual void AccumulateMouseMove(int x, int y) = 0;
  virtual void AccumulateMouseButtonEvent(MouseButton button, bool pressed,
                                          int x, int y) = 0;

  virtual void AccumulateMouseWheelEvent(float angle_in_degrees, int x,
                                         int y) = 0;
  virtual void AccumulateKeyboardEvent(int key, bool pressed) = 0;

  virtual void AccumulateTimeDelta(std::chrono::duration<float> seconds) = 0;

  virtual void Reset() = 0;

  virtual ViewMatrix ProjectionViewMatrix(
      int viewport_width_in_pixels, int viewport_height_in_pixels) const = 0;
};

}  // namespace pure_cpp
}  // namespace reify

#endif  // _REIFY_PURE_CPP_SCENE_VISUALIZER_CAMERA_H_