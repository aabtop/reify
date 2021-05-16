#ifndef _REIFY_SRC_VULKAN_UTILS_INCLUDE_VULKAN_UTILS_WINDOW_RENDERER_H_
#define _REIFY_SRC_VULKAN_UTILS_INCLUDE_VULKAN_UTILS_WINDOW_RENDERER_H_

#include <vulkan/vulkan.h>

#include "platform_window/platform_window.h"
#include "vulkan_utils/swap_chain_renderer.h"
#include "vulkan_utils/vulkan_utils.h"

namespace vulkan_utils {

struct WindowRenderer {
  WithDeleter<VkInstance> instance;
  WithDeleter<VkSurfaceKHR> surface;
  SwapChainRenderer swap_chain_renderer;
};
ErrorOr<WindowRenderer> MakeWindowRenderer(const std::string& application_title,
                                           PlatformWindow window);

}  // namespace vulkan_utils

#endif  // _REIFY_SRC_VULKAN_UTILS_INCLUDE_VULKAN_UTILS_WINDOW_RENDERER_H_
