#include "reify/window/platform_window_wrapper.h"

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

int RunPlatformWindowWrapper(const std::string& window_title,
                             std::unique_ptr<Window> wrapped_window) {
  // Setup a quit_flag so that subsequent processes can signal back to us that
  // they'd like to quit (e.g. if someone presses the close button on the
  // window.)
  reify::utils::ThreadSafeCircularQueue<void, 1> quit_flag;

  // We'll be running all domain visualizer commands on this thread.
  reify::utils::ThreadWithWorkQueue wrapped_window_thread;

  auto set_wrapped_window_size = [&](int32_t width, int32_t height) {
    wrapped_window_thread.Enqueue([&wrapped_window, width, height] {
      wrapped_window->OnViewportResize({width, height});
    });
  };

  auto send_wrapped_window_input_event = [&](const Window::InputEvent& event) {
    wrapped_window_thread.Enqueue(
        [&wrapped_window, event] { wrapped_window->OnInputEvent(event); });
  };

  // Create the window (its message loop will start running in a separate
  // dedicated thread.)
  auto maybe_window = platform_window::Window::Create(
      window_title, [&](PlatformWindowEvent event) {
        // Map from PlatformWindow events into Window events.
        switch (event.type) {
          case kPlatformWindowEventTypeQuitRequest: {
            quit_flag.enqueue();
          } break;
          case kPlatformWindowEventTypeResized: {
            set_wrapped_window_size(event.data.resized.width,
                                    event.data.resized.height);
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
            });
          } break;
        }
      });
  if (!maybe_window) {
    std::cerr << "Error creating window: " << std::endl;
    return 1;
  }
  platform_window::Window& window = *maybe_window;

  // Set the domain visualizer initial width/height.
  set_wrapped_window_size(window.GetWidth(), window.GetHeight());

  // Create the Vulkan renderer.
  auto error_or_renderer =
      MakePlatformWindowRenderer(window_title, window.GetPlatformWindow());
  if (auto error = std::get_if<0>(&error_or_renderer)) {
    std::cerr << "Error creating window Vulkan renderer: " << error->msg
              << std::endl;
    return 1;
  }
  PlatformWindowRenderer& renderer = std::get<1>(error_or_renderer);

  auto error_or_wrapped_window_renderer = wrapped_window->CreateRenderer(
      renderer.instance.value(), renderer.swap_chain_renderer.physical_device(),
      renderer.swap_chain_renderer.device(),
      renderer.swap_chain_renderer.surface_format().format);
  if (auto error = std::get_if<0>(&error_or_wrapped_window_renderer)) {
    std::cerr << "Error creating domain visualizer renderer: " << error->msg
              << std::endl;
    return 1;
  }
  std::unique_ptr<Window::Renderer>& wrapped_window_renderer =
      std::get<1>(error_or_wrapped_window_renderer);

  // Setup a separate dedicated thread to do the rendering.
  reify::utils::ThreadWithWorkQueueLooper<vulkan_utils::Error> render_looper(
      &wrapped_window_thread,
      [&renderer, &wrapped_window_renderer]() {
        return renderer.swap_chain_renderer.Render(
            [&wrapped_window_renderer](
                VkCommandBuffer command_buffer, VkFramebuffer framebuffer,
                const std::array<uint32_t, 2>& output_surface_size) {
              return wrapped_window_renderer->RenderFrame(
                  command_buffer, framebuffer, output_surface_size);
            });
      },
      [&quit_flag](const vulkan_utils::Error& error) {
        std::cerr << "Error rendering frame: " << error.msg << std::endl;
        quit_flag.enqueue();
      });

  window.Show();

  // Wait for a reason to stop and quit.
  quit_flag.dequeue();

  return 0;
}

}  // namespace window
}  // namespace reify
