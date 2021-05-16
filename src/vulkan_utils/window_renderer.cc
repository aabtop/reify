#include "vulkan_utils/window_renderer.h"

#include "platform_window/platform_window.h"
#include "platform_window/vulkan.h"

namespace vulkan_utils {

namespace {

ErrorOr<WithDeleter<VkSurfaceKHR>> MakeWindowSurface(VkInstance instance,
                                                     PlatformWindow window) {
  VkSurfaceKHR surface;
  VkResult err = PlatformWindowVulkanCreateSurface(instance, window, &surface);
  if (err != VK_SUCCESS) {
    return Error{"Error creating Vulkan window surface."};
  }
  return WithDeleter<VkSurfaceKHR>(std::move(surface),
                                   [instance](VkSurfaceKHR&& x) {
                                     vkDestroySurfaceKHR(instance, x, nullptr);
                                   });
}

}  // namespace

ErrorOr<WindowRenderer> MakeWindowRenderer(const std::string& application_title,
                                           PlatformWindow window) {
  VkApplicationInfo app_info{};
  app_info.sType = VK_STRUCTURE_TYPE_APPLICATION_INFO;
  app_info.pApplicationName = application_title.c_str();
  app_info.applicationVersion = VK_MAKE_VERSION(1, 0, 0);
  app_info.pEngineName = "No Engine";
  app_info.engineVersion = VK_MAKE_VERSION(1, 0, 0);
  app_info.apiVersion = VK_API_VERSION_1_0;

  size_t window_extension_count =
      PlatformWindowVulkanGetRequiredInstanceExtensionsCount();
  const char** window_extensions =
      PlatformWindowVulkanGetRequiredInstanceExtensions();
  std::vector<const char*> instance_extensions;
  instance_extensions.reserve(window_extension_count);
  for (size_t i = 0; i < window_extension_count; ++i) {
    instance_extensions.push_back(window_extensions[i]);
  }

  VULKAN_UTILS_ASSIGN_OR_RETURN(instance,
                                MakeInstance(app_info, instance_extensions));

  VULKAN_UTILS_ASSIGN_OR_RETURN(surface,
                                MakeWindowSurface(instance.value(), window));

  VULKAN_UTILS_ASSIGN_OR_RETURN(
      renderer, SwapChainRenderer::Create(instance.value(), surface.value(),
                                          PlatformWindowGetWidth(window),
                                          PlatformWindowGetHeight(window)));

  return WindowRenderer{
      std::move(instance),
      std::move(surface),
      std::move(renderer),
  };
}

}  // namespace vulkan_utils
