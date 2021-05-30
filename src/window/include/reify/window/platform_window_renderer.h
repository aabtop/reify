#ifndef _REIFY_WINDOW_PLATFORM_WINDOW_RENDERER_H_
#define _REIFY_WINDOW_PLATFORM_WINDOW_RENDERER_H_

#include <vulkan/vulkan.h>

#include <optional>

#include "platform_window/platform_window.h"
#include "vulkan_utils/swap_chain_renderer.h"
#include "vulkan_utils/vulkan_utils.h"

namespace reify {
namespace window {

struct PlatformWindowRenderer {
  vulkan_utils::WithDeleter<VkInstance> instance;
  std::optional<vulkan_utils::WithDeleter<VkDebugUtilsMessengerEXT>>
      maybe_debug_utils_messenger;
  vulkan_utils::WithDeleter<VkSurfaceKHR> surface;
  vulkan_utils::SwapChainRenderer swap_chain_renderer;
};
vulkan_utils::ErrorOr<PlatformWindowRenderer> MakePlatformWindowRenderer(
    const std::string& application_title, PlatformWindow window);

}  // namespace window
}  // namespace reify

#endif  // _REIFY_WINDOW_PLATFORM_WINDOW_RENDERER_H_
