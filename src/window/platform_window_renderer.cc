#include "reify/window/platform_window_renderer.h"

#include <iostream>

#include "platform_window/platform_window.h"
#include "platform_window/vulkan.h"
#include "vulkan_utils/vulkan_utils.h"

namespace reify {
namespace window {

namespace {

vulkan_utils::ErrorOr<vulkan_utils::WithDeleter<VkSurfaceKHR>>
MakeWindowSurface(VkInstance instance, PlatformWindow window) {
  VkSurfaceKHR surface;
  VkResult err = PlatformWindowVulkanCreateSurface(instance, window, &surface);
  if (err != VK_SUCCESS) {
    return vulkan_utils::Error{"Error creating Vulkan window surface."};
  }
  return vulkan_utils::WithDeleter<VkSurfaceKHR>(
      std::move(surface), [instance](VkSurfaceKHR&& x) {
        vkDestroySurfaceKHR(instance, x, nullptr);
      });
}

VKAPI_ATTR VkBool32 VKAPI_CALL
LayerMessageCallback(VkDebugUtilsMessageSeverityFlagBitsEXT message_severity,
                     VkDebugUtilsMessageTypeFlagsEXT message_type,
                     const VkDebugUtilsMessengerCallbackDataEXT* callback_data,
                     void* user_data) {
  const char* severity_string = [message_severity] {
    switch (message_severity) {
      case VK_DEBUG_UTILS_MESSAGE_SEVERITY_VERBOSE_BIT_EXT:
        return "verbose";
      case VK_DEBUG_UTILS_MESSAGE_SEVERITY_INFO_BIT_EXT:
        return "info";
      case VK_DEBUG_UTILS_MESSAGE_SEVERITY_WARNING_BIT_EXT:
        return "warning";
      case VK_DEBUG_UTILS_MESSAGE_SEVERITY_ERROR_BIT_EXT:
        return "error";
    };
    return "?";
  }();

  std::cerr << "Vulkan validation layer message (" << severity_string
            << "): " << callback_data->pMessage << std::endl;

  //*((int*)(0)) = 5;

  return VK_FALSE;
}
}  // namespace

vulkan_utils::ErrorOr<PlatformWindowRenderer> MakePlatformWindowRenderer(
    const std::string& application_title, PlatformWindow window) {
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

  VULKAN_UTILS_ASSIGN_OR_RETURN(
      instance, vulkan_utils::MakeInstance(app_info, instance_extensions));

  std::optional<vulkan_utils::WithDeleter<VkDebugUtilsMessengerEXT>>
      maybe_debug_utils_messenger;
  auto error_or_debug_utils_messenger = vulkan_utils::MakeDebugUtilsMessenger(
      instance.value(),
      static_cast<VkDebugUtilsMessageSeverityFlagBitsEXT>(
          VK_DEBUG_UTILS_MESSAGE_SEVERITY_WARNING_BIT_EXT |
          VK_DEBUG_UTILS_MESSAGE_SEVERITY_ERROR_BIT_EXT),
      static_cast<VkDebugUtilsMessageTypeFlagsEXT>(
          VK_DEBUG_UTILS_MESSAGE_TYPE_GENERAL_BIT_EXT |
          VK_DEBUG_UTILS_MESSAGE_TYPE_VALIDATION_BIT_EXT |
          VK_DEBUG_UTILS_MESSAGE_TYPE_PERFORMANCE_BIT_EXT),
      &LayerMessageCallback);
  // Ignore any errors raised here, this is non-essential, and may just mean
  // we're in a production build where validation layers are disabled.
  if (auto debug_utils_messenger =
          std::get_if<1>(&error_or_debug_utils_messenger)) {
    maybe_debug_utils_messenger = std::move(*debug_utils_messenger);
  }

  VULKAN_UTILS_ASSIGN_OR_RETURN(surface,
                                MakeWindowSurface(instance.value(), window));

  VULKAN_UTILS_ASSIGN_OR_RETURN(
      renderer, vulkan_utils::SwapChainRenderer::Create(
                    instance.value(), surface.value(),
                    std::function<std::array<int32_t, 2>()>([window]() {
                      auto size = PlatformWindowGetSize(window);
                      return std::array<int32_t, 2>{size.width, size.height};
                    })));

  return PlatformWindowRenderer{
      std::move(instance),
      std::move(maybe_debug_utils_messenger),
      std::move(surface),
      std::move(renderer),
  };
}

}  // namespace window
}  // namespace reify
