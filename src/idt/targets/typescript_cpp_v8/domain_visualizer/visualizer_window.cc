#include "reify/typescript_cpp_v8/visualizer_window.h"

#include <vulkan/vulkan.h>

#include <condition_variable>
#include <iostream>
#include <mutex>
#include <optional>

#include "platform_window/platform_window.h"
#include "platform_window/vulkan.h"

namespace reify {
namespace typescript_cpp_v8 {

namespace {
VkPhysicalDevice PickBestPhysicalDevice(
    const std::vector<VkPhysicalDevice>& physical_device) {
  assert(!physical_device.empty());
  return physical_device[0];
}

std::optional<uint32_t> FindGraphicsQueueFamilyIndex(
    VkPhysicalDevice physical_device) {
  uint32_t queue_family_count = 0;
  vkGetPhysicalDeviceQueueFamilyProperties(physical_device, &queue_family_count,
                                           nullptr);

  std::vector<VkQueueFamilyProperties> queue_families(queue_family_count);
  vkGetPhysicalDeviceQueueFamilyProperties(physical_device, &queue_family_count,
                                           queue_families.data());

  for (size_t i = 0; i < queue_families.size(); ++i) {
    if (queue_families[i].queueFlags & VK_QUEUE_GRAPHICS_BIT) {
      return static_cast<uint32_t>(i);
    }
  }
  return std::nullopt;
}

struct SwapChainSupportDetails {
  VkSurfaceCapabilitiesKHR capabilities;
  std::vector<VkSurfaceFormatKHR> formats;
  std::vector<VkPresentModeKHR> present_modes;
};

SwapChainSupportDetails QuerySwapChainSupport(VkPhysicalDevice device,
                                              VkSurfaceKHR surface) {
  SwapChainSupportDetails details;
  vkGetPhysicalDeviceSurfaceCapabilitiesKHR(device, surface,
                                            &details.capabilities);
  uint32_t format_count;
  vkGetPhysicalDeviceSurfaceFormatsKHR(device, surface, &format_count, nullptr);

  if (format_count != 0) {
    details.formats.resize(format_count);
    vkGetPhysicalDeviceSurfaceFormatsKHR(device, surface, &format_count,
                                         details.formats.data());
  }

  uint32_t present_mode_count;
  vkGetPhysicalDeviceSurfacePresentModesKHR(device, surface,
                                            &present_mode_count, nullptr);

  if (present_mode_count != 0) {
    details.present_modes.resize(present_mode_count);
    vkGetPhysicalDeviceSurfacePresentModesKHR(
        device, surface, &present_mode_count, details.present_modes.data());
  }

  return details;
}
}  // namespace

int StartVisualizerWindow(const std::string& window_title,
                          std::unique_ptr<DomainVisualizer> domain_visualizer) {
  VkApplicationInfo app_info{};
  app_info.sType = VK_STRUCTURE_TYPE_APPLICATION_INFO;
  app_info.pApplicationName = window_title.c_str();
  app_info.applicationVersion = VK_MAKE_VERSION(1, 0, 0);
  app_info.pEngineName = "No Engine";
  app_info.engineVersion = VK_MAKE_VERSION(1, 0, 0);
  app_info.apiVersion = VK_API_VERSION_1_0;

  VkInstanceCreateInfo instance_create_info{};
  instance_create_info.sType = VK_STRUCTURE_TYPE_INSTANCE_CREATE_INFO;
  instance_create_info.pApplicationInfo = &app_info;
  instance_create_info.enabledExtensionCount =
      PlatformWindowVulkanGetRequiredInstanceExtensionsCount();
  instance_create_info.ppEnabledExtensionNames =
      PlatformWindowVulkanGetRequiredInstanceExtensions();
  instance_create_info.enabledLayerCount = 0;

  VkInstance vulkan_instance;
  VkResult result =
      vkCreateInstance(&instance_create_info, nullptr, &vulkan_instance);
  if (result != VK_SUCCESS) {
    std::cerr << "Error creating Vulkan instance." << std::endl;
    return 1;
  }

  uint32_t device_count = 0;
  vkEnumeratePhysicalDevices(vulkan_instance, &device_count, nullptr);
  if (device_count == 0) {
    std::cerr << "No Vulkan-compatible physical devices found." << std::endl;
    return 1;
  }

  std::vector<VkPhysicalDevice> physical_devices(device_count);
  vkEnumeratePhysicalDevices(vulkan_instance, &device_count,
                             physical_devices.data());

  VkPhysicalDevice physical_device = PickBestPhysicalDevice(physical_devices);

  float queue_priority = 1.0f;
  uint32_t graphics_queue_family_index =
      *FindGraphicsQueueFamilyIndex(physical_device);
  VkDeviceQueueCreateInfo queue_create_info{};
  queue_create_info.sType = VK_STRUCTURE_TYPE_DEVICE_QUEUE_CREATE_INFO;
  queue_create_info.queueFamilyIndex = graphics_queue_family_index;
  queue_create_info.queueCount = 1;
  queue_create_info.pQueuePriorities = &queue_priority;

  const std::vector<const char*> device_extensions = {
      VK_KHR_SWAPCHAIN_EXTENSION_NAME,
  };

  VkPhysicalDeviceFeatures device_features{};
  VkDeviceCreateInfo device_create_info{};
  device_create_info.sType = VK_STRUCTURE_TYPE_DEVICE_CREATE_INFO;
  device_create_info.pQueueCreateInfos = &queue_create_info;
  device_create_info.queueCreateInfoCount = 1;
  device_create_info.pEnabledFeatures = &device_features;
  device_create_info.enabledExtensionCount =
      static_cast<uint32_t>(device_extensions.size());
  device_create_info.ppEnabledExtensionNames = device_extensions.data();
  device_create_info.enabledLayerCount = 0;

  VkDevice device;
  result =
      vkCreateDevice(physical_device, &device_create_info, nullptr, &device);
  if (result != VK_SUCCESS) {
    std::cerr << "Failed to create logical device!" << std::endl;
    return 1;
  }

  VkQueue graphics_queue;
  vkGetDeviceQueue(device, graphics_queue_family_index, 0, &graphics_queue);

  PlatformWindow window = PlatformWindowMakeDefaultWindow(window_title.c_str());
  VkSurfaceKHR vulkan_surface;
  result = PlatformWindowVulkanCreateSurface(vulkan_instance, window,
                                             &vulkan_surface);
  if (result != VK_SUCCESS) {
    std::cerr << "Error creating Vulkan window surface." << std::endl;
    return 1;
  }

  SwapChainSupportDetails swap_chain_support =
      QuerySwapChainSupport(physical_device, vulkan_surface);
  bool swap_chain_adequate = !swap_chain_support.formats.empty() &&
                             !swap_chain_support.present_modes.empty();
  if (!swap_chain_adequate) {
    std::cerr << "No adequate swap chains could be found for this device."
              << std::endl;
    return 1;
  }

  PlatformWindowShow(window);

  while (true) {
    PlatformWindowEvent event = PlatformWindowWaitForNextEvent(window);

    bool should_quit = false;
    switch (event.type) {
      case kPlatformWindowEventTypeQuitRequest: {
        should_quit = true;
      } break;
      case kPlatformWindowEventTypeResized: {
      } break;
      case kPlatformWindowEventTypeNoEvent: {
      } break;
    }

    if (should_quit) {
      break;
    }
  }

  vkDestroySurfaceKHR(vulkan_instance, vulkan_surface, nullptr);
  PlatformWindowDestroyWindow(window);
  vkDestroyDevice(device, nullptr);
  vkDestroyInstance(vulkan_instance, nullptr);

  return 0;
}

}  // namespace typescript_cpp_v8
}  // namespace reify
