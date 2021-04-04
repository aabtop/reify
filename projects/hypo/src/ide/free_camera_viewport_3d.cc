#include "free_camera_viewport_3d.h"

#include <glm/gtc/matrix_access.hpp>
#include <glm/gtc/matrix_transform.hpp>
#include <glm/gtc/quaternion.hpp>

FreeCameraViewport3d::FreeCameraViewport3d(int viewport_width_in_pixels,
                                           int viewport_height_in_pixels)
    : viewport_width_in_pixels_(viewport_width_in_pixels),
      viewport_height_in_pixels_(viewport_height_in_pixels),
      focus_position_(0.0f, 0.0f, 0.0f),
      camera_distance_from_focus_(8.0),
      camera_orientation_(0, 0, 0, 1.0) {
  for (int i = 0; i < static_cast<int>(MouseButton::Count); ++i) {
    mouse_button_pressed_[i] = false;
  }
}

void FreeCameraViewport3d::AccumulateViewportResize(
    int viewport_width_in_pixels, int viewport_height_in_pixels) {
  viewport_width_in_pixels_ = viewport_width_in_pixels;
  viewport_height_in_pixels_ = viewport_height_in_pixels;
  previous_viewport_point_ = std::nullopt;
}

namespace {
glm::quat ArcballRotation(const glm::vec3& new_arcball_point,
                          const glm::vec3& previous_arcball_point) {
  // Find the rotation axis and angle from the previous arcball point to the new
  // one.
  glm::vec3 axis = glm::cross(previous_arcball_point, new_arcball_point);
  float cos_angle = glm::dot(previous_arcball_point, new_arcball_point);
  glm::quat rotation = glm::normalize(glm::quat(cos_angle, axis));

  // Finally apply the rotation from/two the specified amounts.
  return rotation;
}

glm::vec3 FocusPositionTranslation(const glm::vec2& new_viewport_point,
                                   const glm::vec2& previous_viewport_point,
                                   float camera_distance_from_focus,
                                   const glm::quat& orientation) {
  glm::vec2 diff = new_viewport_point - previous_viewport_point;

  // Pull the horizontal and vertical direction vectors from the current view
  // matrix.
  glm::mat4 orientation_matrix = glm::mat4_cast(orientation);
  glm::vec3 right = glm::row(orientation_matrix, 0);
  glm::vec3 up = glm::row(orientation_matrix, 1);

  // Pan the focus position proportional to the zoom level.
  return (right * diff.x + up * diff.y) * camera_distance_from_focus;
}
}  // namespace

void FreeCameraViewport3d::AccumulateMouseMove(int x, int y) {
  // Only adjust the view if the mouse button is pressed.
  if (!previous_viewport_point_) {
    return;
  }

  glm::vec2 new_viewport_point = ToViewportPoint(x, y);
  const glm::vec2& previous_viewport_point = *previous_viewport_point_;

  if (mouse_button_pressed_[static_cast<int>(MouseButton::Left)]) {
    camera_orientation_ = glm::normalize(
        ArcballRotation(ToArcballPoint(new_viewport_point),
                        ToArcballPoint(previous_viewport_point)) *
        camera_orientation_);
  }
  if (mouse_button_pressed_[static_cast<int>(MouseButton::Right)]) {
    focus_position_ += FocusPositionTranslation(
        new_viewport_point, previous_viewport_point,
        camera_distance_from_focus_, camera_orientation_);
  }

  previous_viewport_point_ = new_viewport_point;
}

void FreeCameraViewport3d::AccumulateMouseButtonEvent(MouseButton button,
                                                      bool pressed, int x,
                                                      int y) {
  if (button == MouseButton::Unknown) {
    return;
  }

  mouse_button_pressed_[static_cast<int>(button)] = pressed;

  if (pressed) {
    previous_viewport_point_.emplace(ToViewportPoint(x, y));
  } else {
    bool any_mouse_buttons_pressed = [&buttons_pressed =
                                          mouse_button_pressed_]() {
      for (int i = 0; i < static_cast<int>(MouseButton::Count); ++i) {
        if (buttons_pressed[i]) {
          return true;
        }
      }
    }();
    if (!any_mouse_buttons_pressed) {
      previous_viewport_point_ = std::nullopt;
    }
  }
}

void FreeCameraViewport3d::AccumulateKeyboardEvent(int key, bool pressed) {}

glm::mat4 FreeCameraViewport3d::ViewMatrix() const {
  return glm::translate(
      glm::translate(glm::mat4(1.0f),
                     glm::vec3(0.0f, 0.0f, camera_distance_from_focus_)) *
          glm::mat4_cast(camera_orientation_),
      focus_position_);
}

glm::vec3 FreeCameraViewport3d::ToArcballPoint(
    const glm::vec2& viewport_point) const {
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

glm::vec2 FreeCameraViewport3d::ToViewportPoint(int x, int y) const {
  return glm::vec2(
      (x / static_cast<float>(viewport_width_in_pixels_) - 0.5f) * 2.0f,
      (y / static_cast<float>(viewport_height_in_pixels_) - 0.5f) * -2.0f);
}
