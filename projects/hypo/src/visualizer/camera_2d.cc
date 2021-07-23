#include "camera_2d.h"

#include <glm/gtc/matrix_transform.hpp>

namespace hypo {

Camera2d::Camera2d(int viewport_width_in_pixels, int viewport_height_in_pixels)
    : viewport_width_in_pixels_(viewport_width_in_pixels),
      viewport_height_in_pixels_(viewport_height_in_pixels) {
  Reset();
}

void Camera2d::AccumulateViewportResize(int viewport_width_in_pixels,
                                        int viewport_height_in_pixels) {
  if (viewport_width_in_pixels_ != viewport_width_in_pixels ||
      viewport_height_in_pixels_ != viewport_height_in_pixels) {
    viewport_width_in_pixels_ = viewport_width_in_pixels;
    viewport_height_in_pixels_ = viewport_height_in_pixels;
    previous_viewport_point_ = std::nullopt;
  }
}
void Camera2d::AccumulateMouseMove(int x, int y) {
  // Only adjust the view if the mouse button is pressed.
  if (!previous_viewport_point_) {
    return;
  }

  glm::vec2 new_viewport_point = ToViewportPoint(x, y);

  if (mouse_button_pressed_[static_cast<int>(MouseButton::Right)] ||
      mouse_button_pressed_[static_cast<int>(MouseButton::Left)]) {
    glm::vec2 viewport_diff = new_viewport_point - *previous_viewport_point_;
    viewport_diff.y = -viewport_diff.y;
    const auto virtual_viewport =
        VirtualViewport(viewport_width_in_pixels_, viewport_height_in_pixels_);
    center_ -= glm::vec2(virtual_viewport.width(), virtual_viewport.height()) *
               viewport_diff;
  }

  previous_viewport_point_ = new_viewport_point;
}

void Camera2d::AccumulateMouseButtonEvent(MouseButton button, bool pressed,
                                          int x, int y) {
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
      return false;
    }();
    if (!any_mouse_buttons_pressed) {
      previous_viewport_point_ = std::nullopt;
    }
  }
}

void Camera2d::AccumulateMouseWheelEvent(float angle_in_degrees, int x, int y) {
  // Make it so that as we zoom in, the mouse cursor stays in the same virtual
  // position. We do this by finding the difference in virtual position before
  // and after the zoom.
  glm::vec2 viewport_point = ToViewportPoint(x, y);
  const auto pre_scale_virtual_point = ViewportToVirtualPoint(
      viewport_point, viewport_width_in_pixels_, viewport_height_in_pixels_);

  constexpr float MIN_CAMERA_DISTANCE = 0.001f;
  constexpr float MAX_CAMERA_DISTANCE = 1000.0f;
  scale_ = glm::clamp(scale_ * glm::exp(-angle_in_degrees * 0.005f),
                      MIN_CAMERA_DISTANCE, MAX_CAMERA_DISTANCE);

  const auto post_scale_virtual_point = ViewportToVirtualPoint(
      viewport_point, viewport_width_in_pixels_, viewport_height_in_pixels_);
  center_ -= post_scale_virtual_point - pre_scale_virtual_point;
}

Camera2d::Rect Camera2d::VirtualViewport(int width, int height) const {
  const float aspect_ratio = static_cast<float>(height) / width;
  return Rect{
      -scale_ + center_.x,
      scale_ * aspect_ratio + center_.y,
      scale_ + center_.x,
      -scale_ * aspect_ratio + center_.y,
  };
}

glm::vec2 Camera2d::ViewportToVirtualPoint(const glm::vec2& viewport_point,
                                           int width, int height) const {
  const auto virtual_viewport = VirtualViewport(width, height);
  return glm::vec2(
      virtual_viewport.left + virtual_viewport.width() * viewport_point.x,
      virtual_viewport.bottom +
          virtual_viewport.height() * (1.0f - viewport_point.y));
}

glm::mat4 Camera2d::ProjectionViewMatrix(int viewport_width_in_pixels,
                                         int viewport_height_in_pixels) const {
  const auto virtual_viewport =
      VirtualViewport(viewport_width_in_pixels, viewport_height_in_pixels);
  return
      // Flip the y axis so that positive is up.
      glm::scale(glm::mat4(1.0f), glm::vec3(1.0f, -1.0f, 1.0f)) *
      // Use an orthographic projection.
      glm::ortho(virtual_viewport.left, virtual_viewport.right,
                 virtual_viewport.bottom, virtual_viewport.top);
}

// Reset all view parameters (e.g. camera orientation, position, etc..)
void Camera2d::Reset() {
  center_ = glm::vec2(0, 0);
  scale_ = 100.0f;
}

glm::vec2 Camera2d::ToViewportPoint(int x, int y) const {
  return glm::vec2(static_cast<float>(x) / viewport_width_in_pixels_,
                   static_cast<float>(y) / viewport_height_in_pixels_);
}

}  // namespace hypo
