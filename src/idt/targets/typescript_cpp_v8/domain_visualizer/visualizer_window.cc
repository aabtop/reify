#include "reify/typescript_cpp_v8/visualizer_window.h"

#include <vulkan/vulkan.h>

#include <atomic>
#include <iostream>
#include <optional>

#include "platform_window/platform_window_cpp.h"
#include "reify/utils/thread_safe_circular_queue.h"
#include "reify/utils/thread_with_work_queue.h"
#include "reify/utils/thread_with_work_queue_looper.h"
#include "vulkan_utils/swap_chain_renderer.h"
#include "vulkan_utils/vulkan_utils.h"
#include "vulkan_utils/window_renderer.h"

namespace reify {
namespace typescript_cpp_v8 {

int StartVisualizerWindow(const std::string& window_title,
                          std::unique_ptr<DomainVisualizer> domain_visualizer) {
  // Setup a quit_flag so that subsequent processes can signal back to us that
  // they'd like to quit (e.g. if someone presses the close button on the
  // window.)
  reify::utils::ThreadSafeCircularQueue<void, 1> quit_flag;

  // We'll be running all domain visualizer commands on this thread.
  reify::utils::ThreadWithWorkQueue domain_visualizer_thread;

  auto set_domain_visualizer_size = [&](int32_t width, int32_t height) {
    domain_visualizer_thread.Enqueue([&domain_visualizer, width, height] {
      domain_visualizer->OnViewportResize({width, height});
    });
  };

  auto send_domain_visualizer_input_event =
      [&](const DomainVisualizer::InputEvent& event) {
        domain_visualizer_thread.Enqueue([&domain_visualizer, event] {
          domain_visualizer->OnInputEvent(event);
        });
      };

  // Create the window (its message loop will start running in a separate
  // dedicated thread.)
  auto maybe_window = platform_window::Window::Create(
      window_title, [&](PlatformWindowEvent event) {
        switch (event.type) {
          case kPlatformWindowEventTypeQuitRequest: {
            quit_flag.enqueue();
          } break;
          case kPlatformWindowEventTypeResized: {
            set_domain_visualizer_size(event.data.resized.width,
                                       event.data.resized.height);
          } break;
          case kPlatformWindowEventTypeMouseMove: {
            send_domain_visualizer_input_event(DomainVisualizer::MouseMoveEvent{
                event.data.mouse_move.x,
                event.data.mouse_move.y,
            });
          } break;
          case kPlatformWindowEventTypeMouseButton: {
            send_domain_visualizer_input_event(
                DomainVisualizer::MouseButtonEvent{
                    [button = event.data.mouse_button.button] {
                      switch (button) {
                        case kPlatformWindowMouseLeft:
                          return DomainVisualizer::MouseButton::Left;
                        case kPlatformWindowMouseRight:
                          return DomainVisualizer::MouseButton::Right;
                        default:
                          return DomainVisualizer::MouseButton::Unknown;
                      }
                    }(),
                    event.data.mouse_button.pressed,
                    event.data.mouse_button.x,
                    event.data.mouse_button.y,
                });
          } break;
          case kPlatformWindowEventTypeMouseWheel: {
            send_domain_visualizer_input_event(
                DomainVisualizer::MouseWheelEvent{
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
  set_domain_visualizer_size(window.GetWidth(), window.GetHeight());

  // Create the Vulkan renderer.
  auto error_or_renderer = vulkan_utils::MakeWindowRenderer(
      window_title, window.GetPlatformWindow());
  if (auto error = std::get_if<0>(&error_or_renderer)) {
    std::cerr << "Error creating window Vulkan renderer: " << error->msg
              << std::endl;
    return 1;
  }
  vulkan_utils::WindowRenderer& renderer = std::get<1>(error_or_renderer);

  auto error_or_domain_visualizer_renderer = domain_visualizer->CreateRenderer(
      renderer.instance.value(), renderer.swap_chain_renderer.physical_device(),
      renderer.swap_chain_renderer.device(),
      renderer.swap_chain_renderer.surface_format().format);
  if (auto error = std::get_if<0>(&error_or_domain_visualizer_renderer)) {
    std::cerr << "Error creating domain visualizer renderer: " << error->msg
              << std::endl;
    return 1;
  }
  std::unique_ptr<DomainVisualizer::Renderer>& domain_visualizer_renderer =
      std::get<1>(error_or_domain_visualizer_renderer);

  // Setup a separate dedicated thread to do the rendering.
  reify::utils::ThreadWithWorkQueueLooper<vulkan_utils::Error> render_looper(
      &domain_visualizer_thread,
      [&renderer, &domain_visualizer_renderer]() {
        return renderer.swap_chain_renderer.Render(
            [&domain_visualizer_renderer](
                VkCommandBuffer command_buffer, VkFramebuffer framebuffer,
                const std::array<uint32_t, 2>& output_surface_size) {
              return domain_visualizer_renderer->RenderFrame(
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

}  // namespace typescript_cpp_v8
}  // namespace reify
