#include "reify/window/platform_window_wrapper.h"

#include <fmt/format.h>
#include <vulkan/vulkan.h>

#include <atomic>
#include <iostream>
#include <optional>

#include "platform_window/platform_window_cpp.h"
#include "reify/utils/thread_safe_circular_queue.h"
#include "reify/utils/thread_with_work_queue.h"
#include "reify/utils/thread_with_work_queue_looper.h"
#include "reify/window/platform_window_renderer.h"
#include "reify/window/window.h"
#include "vulkan_utils/swap_chain_renderer.h"
#include "vulkan_utils/vulkan_utils.h"

namespace reify {
namespace window {

utils::MaybeError RunPlatformWindowWrapper(
    const std::string& window_title, Window* wrapped_window,
    utils::ThreadWithWorkQueue* wrapped_window_thread) {
  // Setup a quit_flag so that subsequent processes can signal back to us that
  // they'd like to quit (e.g. if someone presses the close button on the
  // window.)
  reify::utils::ThreadSafeCircularQueue<utils::MaybeError, 1> quit_flag;

  // We'll be running all window commands on `wrapped_window_thread`.

  auto set_wrapped_window_size = [&](int32_t width, int32_t height) {
    wrapped_window_thread->Enqueue([wrapped_window, width, height] {
      wrapped_window->OnViewportResize({width, height});
    });
  };

  auto send_wrapped_window_input_event = [&](const Window::InputEvent& event) {
    wrapped_window_thread->Enqueue(
        [wrapped_window, event] { wrapped_window->OnInputEvent(event); });
  };

  // Create the window (its message loop will start running in a separate
  // dedicated thread.)
  auto maybe_window = platform_window::Window::Create(
      window_title, [&](PlatformWindowEvent event) {
        // Map from PlatformWindow events into Window events.
        switch (event.type) {
          case kPlatformWindowEventTypeQuitRequest: {
            quit_flag.enqueue(std::nullopt);
          } break;
          case kPlatformWindowEventTypeResized: {
            set_wrapped_window_size(event.data.resized.size.width,
                                    event.data.resized.size.height);
          } break;
          case kPlatformWindowEventTypeMouseMove: {
            send_wrapped_window_input_event(Window::MouseMoveEvent{
                event.data.mouse_move.x,
                event.data.mouse_move.y,
            });
          } break;
          case kPlatformWindowEventTypeMouseButton: {
            send_wrapped_window_input_event(Window::MouseButtonEvent{
                [button = event.data.mouse_button.button] {
                  switch (button) {
                    case kPlatformWindowMouseLeft:
                      return Window::MouseButton::Left;
                    case kPlatformWindowMouseRight:
                      return Window::MouseButton::Right;
                    default:
                      return Window::MouseButton::Unknown;
                  }
                }(),
                event.data.mouse_button.pressed,
                event.data.mouse_button.x,
                event.data.mouse_button.y,
            });
          } break;
          case kPlatformWindowEventTypeMouseWheel: {
            send_wrapped_window_input_event(Window::MouseWheelEvent{
                event.data.mouse_wheel.angle_in_degrees,
                event.data.mouse_wheel.x,
                event.data.mouse_wheel.y,
            });
          } break;
          case kPlatformWindowEventTypeKey: {
            send_wrapped_window_input_event(Window::KeyboardEvent{
                event.data.key.key,
                event.data.key.pressed,
            });
          } break;
          case kPlatformWindowEventTypeNoEvent:
            break;
        }
      });
  if (!maybe_window) {
    return utils::Error{"Error creating window."};
  }
  platform_window::Window& window = *maybe_window;

  // Set the window initial width/height.
  PlatformWindowSize initial_size = window.GetSize();
  set_wrapped_window_size(initial_size.width, initial_size.height);

  // Create the Vulkan renderer.
  auto error_or_renderer =
      MakePlatformWindowRenderer(window_title, window.GetPlatformWindow());
  if (auto error = std::get_if<0>(&error_or_renderer)) {
    return utils::Error{error->msg};
  }
  PlatformWindowRenderer& renderer = std::get<1>(error_or_renderer);

  auto error_or_wrapped_window_renderer = wrapped_window->CreateRenderer(
      renderer.instance.value(), renderer.swap_chain_renderer.physical_device(),
      renderer.swap_chain_renderer.device(),
      renderer.swap_chain_renderer.surface_format().format);
  if (auto error = std::get_if<0>(&error_or_wrapped_window_renderer)) {
    return utils::Error{error->msg};
  }
  std::unique_ptr<Window::Renderer>& wrapped_window_renderer =
      std::get<1>(error_or_wrapped_window_renderer);

  // Setup a separate dedicated thread to do the rendering.
  std::optional<std::chrono::time_point<std::chrono::high_resolution_clock>>
      previous_time = std::chrono::high_resolution_clock::now();

  std::optional<reify::utils::ThreadWithWorkQueueLooper<vulkan_utils::Error>>
      render_looper(
          std::in_place, wrapped_window_thread,
          [&renderer, &wrapped_window_renderer, &previous_time,
           &wrapped_window]() -> std::optional<vulkan_utils::Error> {
            auto result = renderer.swap_chain_renderer.Render(
                [&wrapped_window_renderer, &previous_time, &wrapped_window](
                    VkCommandBuffer command_buffer, VkFramebuffer framebuffer,
                    VkImage output_color_image,
                    const std::array<uint32_t, 2>& output_surface_size) {
                  auto current_time = std::chrono::high_resolution_clock::now();
                  if (previous_time) {
                    wrapped_window->AdvanceTime(current_time - *previous_time);
                  }
                  previous_time = current_time;
                  return wrapped_window_renderer->RenderFrame(
                      command_buffer, framebuffer, output_color_image,
                      {0, 0, static_cast<int>(output_surface_size[0]),
                       static_cast<int>(output_surface_size[1])});
                });
            if (result && result->vk_result == VK_ERROR_OUT_OF_DATE_KHR) {
              // Ignore swap chain out of date errors, the swap chain renderer
              // will update to use a new swap chain next time we render with
              // it.
              return std::nullopt;
            }
            return result;
          },
          [&quit_flag](const vulkan_utils::Error& error) {
            quit_flag.enqueue(utils::Error{
                fmt::format("Error rendering frame: {}", error.msg)});
          });

  window.Show();

  // Wait for a reason to stop and quit.
  auto quit_result = quit_flag.dequeue();

  // Flush out draw commands before we shut things down, ensuring that the
  // resources about to be shutdown are no longer in use.
  render_looper = std::nullopt;
  wrapped_window_thread
      ->EnqueueWithResult<bool>(
          [device = renderer.swap_chain_renderer.device()] {
            vkDeviceWaitIdle(device);
            return true;
          })
      .wait_and_get_results();

  return quit_result;
}

}  // namespace window
}  // namespace reify
