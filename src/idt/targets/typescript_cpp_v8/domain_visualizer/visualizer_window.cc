#include "reify/typescript_cpp_v8/visualizer_window.h"

#include <vulkan/vulkan.h>

#include <iostream>
#include <optional>

#include "platform_window/platform_window.h"
#include "vulkan_utils/swap_chain_renderer.h"
#include "vulkan_utils/vulkan_utils.h"
#include "vulkan_utils/window_renderer.h"

namespace reify {
namespace typescript_cpp_v8 {

int StartVisualizerWindow(const std::string& window_title,
                          std::unique_ptr<DomainVisualizer> domain_visualizer) {
  vulkan_utils::WithDeleter<PlatformWindow> window(
      PlatformWindowMakeDefaultWindow(window_title.c_str()),
      [](PlatformWindow&& x) { PlatformWindowDestroyWindow(x); });

  vulkan_utils::ErrorOr<vulkan_utils::WindowRenderer> error_or_renderer =
      vulkan_utils::MakeWindowRenderer(window_title, window.value());
  if (auto error = std::get_if<0>(&error_or_renderer)) {
    std::cerr << "Error creating Vulkan renderer: " << error->msg << std::endl;
    return 1;
  }
  vulkan_utils::WindowRenderer& renderer = std::get<1>(error_or_renderer);

  // Prepare to spin up the main loop.
  bool window_visible = false;
  PlatformWindowEnqueueCustomEvent(window.value(), {});
  while (true) {
    PlatformWindowEvent event = PlatformWindowWaitForNextEvent(window.value());

    bool should_quit = false;
    switch (event.type) {
      case kPlatformWindowEventTypeQuitRequest: {
        should_quit = true;
      } break;
      case kPlatformWindowEventTypeResized: {
      } break;
      case kPlatformWindowEventTypeNoEvent: {
      } break;
      case kPlatformWindowEventTypeCustom: {
        // There's only one custom event currently, which is assumed to mean
        // that it is time to render a frame.
        auto maybe_error = renderer.swap_chain_renderer.Render(
            [](VkCommandBuffer command_buffer, VkFramebuffer framebuffer,
               const std::array<uint32_t, 2>& output_surface_size) {
              return vulkan_utils::FrameResources();
            });
        if (maybe_error) {
          std::cerr << "Error while rendering a frame: " << maybe_error->msg
                    << std::endl;
          return 1;
        }

        PlatformWindowShow(window.value());
        PlatformWindowEnqueueCustomEvent(window.value(), {});
      } break;
    }

    if (should_quit) {
      break;
    }
  }

  return 0;
}

}  // namespace typescript_cpp_v8
}  // namespace reify
