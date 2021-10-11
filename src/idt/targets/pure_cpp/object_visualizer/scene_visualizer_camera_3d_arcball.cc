#include "reify/pure_cpp/scene_visualizer_camera_3d_arcball.h"

#include <glm/gtc/matrix_access.hpp>
#include <glm/gtc/matrix_transform.hpp>
#include <glm/gtc/quaternion.hpp>

#ifndef NDEBUG
#include <glm/gtx/string_cast.hpp>
#include <iostream>
#endif

namespace reify {
namespace pure_cpp {

// Just a really nice viewing angle.
constexpr glm::quat INITIAL_ROTATION(0.490635, glm::vec3(-0.758228f, 0.364142f,
                                                         0.227527f));

SceneVisualizerCamera3dArcball::SceneVisualizerCamera3dArcball(
    int viewport_width_in_pixels, int viewport_height_in_pixels)
    : viewport_width_in_pixels_(viewport_width_in_pixels),
      viewport_height_in_pixels_(viewport_height_in_pixels) {
  for (int i = 0; i < static_cast<int>(MouseButton::Count); ++i) {
    mouse_button_pressed_[i] = false;
  }
  Reset();
}

void SceneVisualizerCamera3dArcball::Reset() {
  focus_position_ = glm::vec3(0.0f, 0.0f, 0.0f);
  camera_distance_from_focus_ = 20.0;
  camera_orientation_ = INITIAL_ROTATION;
}

void SceneVisualizerCamera3dArcball::AccumulateViewportResize(
    int viewport_width_in_pixels, int viewport_height_in_pixels) {
  if (viewport_width_in_pixels_ != viewport_width_in_pixels ||
      viewport_height_in_pixels_ != viewport_height_in_pixels) {
    viewport_width_in_pixels_ = viewport_width_in_pixels;
    viewport_height_in_pixels_ = viewport_height_in_pixels;
    previous_viewport_point_ = std::nullopt;
  }
}

namespace {
glm::quat ArcballRotation(const glm::vec3& new_arcball_point,
                          const glm::vec3& previous_arcball_point) {
  // Find the rotation axis and angle from the previous arcball point to the new
  // one.
  glm::vec3 axis = glm::cross(previous_arcball_point, new_arcball_point);
  float cos_angle = glm::dot(previous_arcball_point, new_arcball_point);
  glm::quat rotation = glm::normalize(glm::quat(cos_angle * 2, axis));

  // Finally apply the rotation from/two the specified amounts.
  return rotation;
}

glm::vec3 FocusPositionTranslationFromMousePan(
    const glm::vec2& new_viewport_point,
    const glm::vec2& previous_viewport_point, float camera_distance_from_focus,
    const glm::quat& orientation) {
  glm::vec2 diff = new_viewport_point - previous_viewport_point;

  // Pull the horizontal and vertical direction vectors from the current view
  // matrix.
  glm::mat4 orientation_matrix = glm::mat4_cast(orientation);
  glm::vec3 right = glm::row(orientation_matrix, 0);
  glm::vec3 up = glm::row(orientation_matrix, 1);

  // Pan the focus position proportional to the zoom level. We negate the result
  // because as we drag the mouse to the right, we want the *object* to appear
  // to move to the right, and so the focus point must move to the left.
  return -(right * diff.x + up * diff.y) * camera_distance_from_focus;
}
}  // namespace

void SceneVisualizerCamera3dArcball::AccumulateMouseMove(int x, int y) {
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
    // This shouldn't ever happen... but it has happened in the past, and
    // it was fixed, but it wasn't a great user experience and it wasn't fun
    // to fix it without some debug checks.
    if (std::isnan(camera_orientation_.w)) {
#ifdef NDEBUG
      camera_orientation_ = INITIAL_ROTATION;
#else
      std::cerr << "Invalid camera rotation." << std::endl;
      std::cerr << "camera_orientation_: "
                << glm::to_string(camera_orientation_) << std::endl;
      std::cerr << "new_viewport_point: " << glm::to_string(new_viewport_point)
                << std::endl;
      std::cerr << "previous_viewport_point: "
                << glm::to_string(previous_viewport_point) << std::endl;
      std::cerr << "ToArcballPoint(new_viewport_point): "
                << glm::to_string(ToArcballPoint(new_viewport_point))
                << std::endl;
      std::cerr << "ToArcballPoint(previous_viewport_point)): "
                << glm::to_string(ToArcballPoint(previous_viewport_point))
                << std::endl;
      assert(false);
#endif
    }
  }
  if (mouse_button_pressed_[static_cast<int>(MouseButton::Right)]) {
    focus_position_ += FocusPositionTranslationFromMousePan(
        new_viewport_point, previous_viewport_point,
        camera_distance_from_focus_, camera_orientation_);
  }

  previous_viewport_point_ = new_viewport_point;
}

void SceneVisualizerCamera3dArcball::AccumulateMouseButtonEvent(
    MouseButton button, bool pressed, int x, int y) {
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

void SceneVisualizerCamera3dArcball::AccumulateMouseWheelEvent(
    float angle_in_degrees, int x, int y) {
  constexpr float MIN_CAMERA_DISTANCE = 0.001f;
  constexpr float MAX_CAMERA_DISTANCE = 1000.0f;
  camera_distance_from_focus_ = glm::clamp(
      camera_distance_from_focus_ * glm::exp(-angle_in_degrees * 0.005f),
      MIN_CAMERA_DISTANCE, MAX_CAMERA_DISTANCE);
}

void SceneVisualizerCamera3dArcball::AccumulateKeyboardEvent(int key,
                                                             bool pressed) {
  if (pressed) {
    keys_pressed_.insert(key);
  } else {
    keys_pressed_.erase(key);
  }
}

glm::mat4 SceneVisualizerCamera3dArcball::ProjectionViewMatrix(
    int viewport_width_in_pixels, int viewport_height_in_pixels) const {
  const glm::mat4 perspective_projection_matrix =
      glm::infinitePerspective(
          45.0f,
          (viewport_width_in_pixels) /
              static_cast<float>(viewport_height_in_pixels),
          0.1f)
      // Flip the y and z axes so that positive y is up and positive z is
      // away.
      * glm::scale(glm::mat4(1), glm::vec3(1.0f, -1.0f, -1.0f));

  return perspective_projection_matrix *
         glm::translate(glm::translate(glm::mat4(1.0f),
                                       glm::vec3(0.0f, 0.0f,
                                                 camera_distance_from_focus_)) *
                            glm::mat4_cast(camera_orientation_),
                        -focus_position_);
}

namespace {
glm::vec3 PositionDeltaFromPressedKeys(
    const std::unordered_set<int>& keys_pressed,
    const glm::quat& camera_orientation, float camera_distance_from_focus,
    std::chrono::duration<float> time_delta) {
  auto key_is_pressed = [&keys_pressed](int key) {
    return keys_pressed.find(key) != keys_pressed.end();
  };

  const glm::mat4 orientation_matrix = glm::mat4_cast(camera_orientation);

  glm::vec3 velocity(0.0f, 0.0f, 0.0f);
  if (key_is_pressed(0x41)) {
    // The 'a' key is pressed.
    velocity += -glm::vec3(glm::row(orientation_matrix, 0));
  }
  if (key_is_pressed(0x53)) {
    // The 's' key is pressed.
    velocity += -glm::vec3(glm::row(orientation_matrix, 2));
  }
  if (key_is_pressed(0x44)) {
    // The 'd' key is pressed.
    velocity += glm::vec3(glm::row(orientation_matrix, 0));
  }
  if (key_is_pressed(0x57)) {
    // The 'w' key is pressed.
    velocity += glm::vec3(glm::row(orientation_matrix, 2));
  }

  // Now scale the velocity both by how much time has passed and by how
  // far the focal point is from the camera (i.e. how zoomed in we are).
  constexpr float SPEED = 1.0f;
  glm::vec3 position_delta =
      velocity * SPEED * time_delta.count() * camera_distance_from_focus;
  return position_delta;
}
}  // namespace

void SceneVisualizerCamera3dArcball::AccumulateTimeDelta(
    std::chrono::duration<float> time_delta) {
  focus_position_ +=
      PositionDeltaFromPressedKeys(keys_pressed_, camera_orientation_,
                                   camera_distance_from_focus_, time_delta);
}

glm::vec3 SceneVisualizerCamera3dArcball::ToArcballPoint(
    const glm::vec2& viewport_point) const {
  float length = glm::length(viewport_point);

  // A little less than 1.0f so that if the user moves their mouse inside the
  // viewport but near its extremities, they can still rotate the roll.
  constexpr float ARCBALL_RADIUS = 0.8f;
  if (length > ARCBALL_RADIUS) {
    // We're outside of our virtual sphere, so project onto the edge of it.
    return glm::vec3(viewport_point.x / length, viewport_point.y / length, 0);
  }

  const float arcball_normalized_length = length / ARCBALL_RADIUS;
  // Avoid a potential division by zero later on.
  if (arcball_normalized_length == 0.0f) {
    return glm::vec3(0, 0, -1);
  }

  const float smoothed_length = [](auto x) {
    // This is the solution of a cubic function `f(x) = x + x^2 - x^3` where:
    // f(0)  = 0
    // f'(0) = 1
    // f(1)  = 1
    // f'(1) = 0
    // The idea is to smooth things out as the user moves their mouse from
    // outside of the ARCBALL_RADIUS to inside of it, otherwise it can be
    // a little jarring. We do so by transforming their mouse cursor length
    // through a function that has a derivative of 0 (equal to how things work
    // above when we're outside of the radius) at length == ARCBALL_RADIUS.
    return x + x * x - x * x * x;
  }(arcball_normalized_length);

  const float smoothed_multiplier = smoothed_length / arcball_normalized_length;

  const float smoothed_x = viewport_point.x * smoothed_multiplier;
  const float smoothed_y = viewport_point.y * smoothed_multiplier;

  const float normalized_x = smoothed_x / ARCBALL_RADIUS;
  const float normalized_y = smoothed_y / ARCBALL_RADIUS;

  // In case any floating point error puts us above 1, clamp it to 1.
  const float z_squared = std::max(
      0.0f, 1.0f - (normalized_x * normalized_x + normalized_y * normalized_y));

  // Project the 2D point onto the surface of the virtual arcball sphere
  // centered at the origin.
  return glm::vec3(normalized_x, normalized_y, -glm::sqrt(z_squared));
}

glm::vec2 SceneVisualizerCamera3dArcball::ToViewportPoint(int x, int y) const {
  const float x_multiplier = static_cast<float>(
      viewport_width_in_pixels_ < viewport_height_in_pixels_
          ? 1
          : viewport_width_in_pixels_ / viewport_height_in_pixels_);
  const float y_multiplier = static_cast<float>(
      viewport_width_in_pixels_ > viewport_height_in_pixels_
          ? 1
          : viewport_height_in_pixels_ / viewport_width_in_pixels_);
  return glm::vec2((x / static_cast<float>(viewport_width_in_pixels_) - 0.5f) *
                       2.0f * x_multiplier,
                   (y / static_cast<float>(viewport_height_in_pixels_) - 0.5f) *
                       -2.0f * y_multiplier);
}

}  // namespace pure_cpp
}  // namespace reify