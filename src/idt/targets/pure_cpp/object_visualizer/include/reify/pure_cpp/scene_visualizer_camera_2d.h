#ifndef _REIFY_PURE_CPP_SCENE_VISUALIZER_CAMERA_2D_H_
#define _REIFY_PURE_CPP_SCENE_VISUALIZER_CAMERA_2D_H_

#include <glm/glm.hpp>
#include <optional>

#include "reify/pure_cpp/scene_visualizer_camera.h"
#include "reify/window/window.h"

namespace reify {
namespace pure_cpp {

class SceneVisualizerCamera2d : public SceneVisualizerCamera<glm::mat4> {
 public:
  SceneVisualizerCamera2d(int viewport_width_in_pixels,
                          int viewport_height_in_pixels);

  void AccumulateViewportResize(int viewport_width_in_pixels,
                                int viewport_height_in_pixels) override;
  void AccumulateMouseMove(int x, int y) override;
  void AccumulateMouseButtonEvent(MouseButton button, bool pressed, int x,
                                  int y) override;

  void AccumulateMouseWheelEvent(float angle_in_degrees, int x, int y) override;
  void AccumulateKeyboardEvent(int key, bool pressed) override {}

  void AccumulateTimeDelta(std::chrono::duration<float> seconds) override {}

  // Reset all view parameters (e.g. camera orientation, position, etc..)
  void Reset() override;

  glm::mat4 ProjectionViewMatrix(int viewport_width_in_pixels,
                                 int viewport_height_in_pixels) const override;

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

}  // namespace pure_cpp
}  // namespace reify

#endif  // _REIFY_PURE_CPP_SCENE_VISUALIZER_CAMERA_2D_H_
