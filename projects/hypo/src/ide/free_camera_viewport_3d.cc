#include "free_camera_viewport_3d.h"

#include <glm/gtc/matrix_transform.hpp>
#include <glm/gtc/quaternion.hpp>

FreeCameraViewport3d::FreeCameraViewport3d(int viewport_width_in_pixels,
                                           int viewport_height_in_pixels)
    : viewport_width_in_pixels_(viewport_width_in_pixels),
      viewport_height_in_pixels_(viewport_height_in_pixels),
      focus_position_(0.0f, 0.0f, 0.0f),
      camera_distance_from_focus_(8.0) {}

void FreeCameraViewport3d::AccumulateViewportResize(
    int viewport_width_in_pixels, int viewport_height_in_pixels) {
  viewport_width_in_pixels_ = viewport_width_in_pixels;
  viewport_height_in_pixels_ = viewport_height_in_pixels;
  previous_arcball_point_ = std::nullopt;
}

void FreeCameraViewport3d::AccumulateMouseMove(int x, int y) {
  // Only adjust the view if the mouse button is pressed.
  if (!previous_arcball_point_) {
    return;
  }

  glm::vec3 new_arcball_point = ToArcballPoint(x, y);

  // Find the rotation axis and angle from the previous arcball point to the new
  // one.
  glm::vec3 axis = glm::cross(*previous_arcball_point_, new_arcball_point);
  float cos_angle = glm::dot(*previous_arcball_point_, new_arcball_point);
  glm::quat rotation = glm::normalize(glm::quat(cos_angle, axis));

  // Finally apply the rotation from/two the specified amounts.
  camera_orientation_ = glm::normalize(rotation * camera_orientation_);

  previous_arcball_point_ = new_arcball_point;
}

void FreeCameraViewport3d::AccumulateMouseButtonEvent(MouseButton button,
                                                      bool pressed, int x,
                                                      int y) {
  if (button == MouseButton::Left) {
    if (pressed) {
      previous_arcball_point_.emplace(ToArcballPoint(x, y));
    } else {
      previous_arcball_point_ = std::nullopt;
    }
  }
}

void FreeCameraViewport3d::AccumulateKeyboardEvent(int key, bool pressed) {}

glm::vec3 FreeCameraViewport3d::ToArcballPoint(int x, int y) {
  glm::vec2 viewport_point(
      (x / static_cast<float>(viewport_width_in_pixels_) - 0.5f) * 2.0f,
      (y / static_cast<float>(viewport_height_in_pixels_) - 0.5f) * -2.0f);

  float length = glm::length(viewport_point);

  constexpr float ARCBALL_RADIUS = 1.0f;
  if (length > ARCBALL_RADIUS) {
    // We're outside of our virtual sphere, so project onto the edge of it.
    return glm::vec3(viewport_point.x / length, viewport_point.y / length, 0);
  }

  // Project the 2D point onto the surface of the virtual arcball sphere
  // centered at the origin.
  return glm::vec3(viewport_point.x, viewport_point.y,
                   -glm::sqrt(1 - viewport_point.x * viewport_point.x -
                              viewport_point.y * viewport_point.y));
}

glm::mat4 FreeCameraViewport3d::ViewMatrix() const {
  return glm::translate(
      glm::translate(glm::mat4(1.0f),
                     glm::vec3(0.0f, 0.0f, camera_distance_from_focus_)) *
          glm::mat4_cast(camera_orientation_),
      focus_position_);
}