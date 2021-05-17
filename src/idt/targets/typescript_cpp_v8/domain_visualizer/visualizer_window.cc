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

  // Create the window (its message loop will start running in a separate
  // dedicated thread.)
  auto maybe_window = platform_window::Window::Create(
      window_title, [&quit_flag](PlatformWindowEvent event) {
        switch (event.type) {
          case kPlatformWindowEventTypeQuitRequest: {
            quit_flag.enqueue();
          } break;
        }
      });
  if (!maybe_window) {
    std::cerr << "Error creating window: " << std::endl;
    return 1;
  }
  platform_window::Window& window = *maybe_window;

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
  reify::utils::ThreadWithWorkQueue render_repeater_thread;
  reify::utils::ThreadWithWorkQueueLooper<vulkan_utils::Error> render_looper(
      &render_repeater_thread,
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
