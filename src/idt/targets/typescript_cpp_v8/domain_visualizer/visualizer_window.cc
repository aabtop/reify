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

std::vector<VkQueueFamilyProperties> GetQueueFamilies(
    VkPhysicalDevice physical_device) {
  uint32_t queue_family_count = 0;
  vkGetPhysicalDeviceQueueFamilyProperties(physical_device, &queue_family_count,
                                           nullptr);

  std::vector<VkQueueFamilyProperties> queue_families(queue_family_count);
  vkGetPhysicalDeviceQueueFamilyProperties(physical_device, &queue_family_count,
                                           queue_families.data());
  return queue_families;
}

std::optional<uint32_t> FindFirstQueueFamilyIndex(
    VkPhysicalDevice physical_device, VkQueueFlagBits queue_family_bits) {
  std::vector<VkQueueFamilyProperties> queue_families =
      GetQueueFamilies(physical_device);

  for (size_t i = 0; i < queue_families.size(); ++i) {
    if (queue_families[i].queueFlags & queue_family_bits) {
      return static_cast<uint32_t>(i);
    }
  }

  return std::nullopt;
}

std::optional<uint32_t> FindFirstPresentQueueFamilyIndex(
    VkPhysicalDevice physical_device, VkSurfaceKHR surface) {
  uint32_t queue_family_count = 0;
  vkGetPhysicalDeviceQueueFamilyProperties(physical_device, &queue_family_count,
                                           nullptr);

  for (uint32_t i = 0; i < queue_family_count; ++i) {
    VkBool32 present_support;
    vkGetPhysicalDeviceSurfaceSupportKHR(physical_device, i, surface,
                                         &present_support);
    if (present_support) {
      return i;
    }
  }
  return std::nullopt;
}

struct SwapChainSupportDetails {
  VkSurfaceCapabilitiesKHR capabilities;
  std::vector<VkSurfaceFormatKHR> formats;
  std::vector<VkPresentModeKHR> present_modes;
};

SwapChainSupportDetails QuerySwapChainSupport(VkPhysicalDevice physical_device,
                                              VkSurfaceKHR surface) {
  SwapChainSupportDetails details;
  vkGetPhysicalDeviceSurfaceCapabilitiesKHR(physical_device, surface,
                                            &details.capabilities);
  uint32_t format_count;
  vkGetPhysicalDeviceSurfaceFormatsKHR(physical_device, surface, &format_count,
                                       nullptr);

  if (format_count != 0) {
    details.formats.resize(format_count);
    vkGetPhysicalDeviceSurfaceFormatsKHR(physical_device, surface,
                                         &format_count, details.formats.data());
  }

  uint32_t present_mode_count;
  vkGetPhysicalDeviceSurfacePresentModesKHR(physical_device, surface,
                                            &present_mode_count, nullptr);

  if (present_mode_count != 0) {
    details.present_modes.resize(present_mode_count);
    vkGetPhysicalDeviceSurfacePresentModesKHR(physical_device, surface,
                                              &present_mode_count,
                                              details.present_modes.data());
  }

  return details;
}

VkSurfaceFormatKHR ChooseSwapSurfaceFormat(
    const std::vector<VkSurfaceFormatKHR>& available_formats) {
  for (const auto& available_format : available_formats) {
    if (available_format.format == VK_FORMAT_B8G8R8A8_SRGB &&
        available_format.colorSpace == VK_COLOR_SPACE_SRGB_NONLINEAR_KHR) {
      return available_format;
    }
  }

  return available_formats[0];
}

VkPresentModeKHR ChooseSwapPresentMode(const std::vector<VkPresentModeKHR>&) {
  // The VK_PRESENT_MODE_FIFO_KHR present mode is guaranteed to be avaialble.
  return VK_PRESENT_MODE_FIFO_KHR;
}

VkExtent2D ChooseSwapExtent(const VkSurfaceCapabilitiesKHR& capabilities,
                            PlatformWindow window) {
  if (capabilities.currentExtent.width != UINT32_MAX) {
    return capabilities.currentExtent;
  } else {
    std::cerr << "Not-well-tested-code alert: Pixels != screen coordinates."
              << std::endl;

    int32_t width = PlatformWindowGetWidth(window);
    int32_t height = PlatformWindowGetHeight(window);

    VkExtent2D actual_extent = {static_cast<uint32_t>(width),
                                static_cast<uint32_t>(height)};

    actual_extent.width = std::max(
        capabilities.minImageExtent.width,
        std::min(capabilities.maxImageExtent.width, actual_extent.width));
    actual_extent.height = std::max(
        capabilities.minImageExtent.height,
        std::min(capabilities.maxImageExtent.height, actual_extent.height));

    return actual_extent;
  }
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
      *FindFirstQueueFamilyIndex(physical_device, VK_QUEUE_GRAPHICS_BIT);
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
  VkSurfaceKHR surface;
  result = PlatformWindowVulkanCreateSurface(vulkan_instance, window, &surface);
  if (result != VK_SUCCESS) {
    std::cerr << "Error creating Vulkan window surface." << std::endl;
    return 1;
  }

  // Create the swapchain.
  SwapChainSupportDetails swap_chain_support =
      QuerySwapChainSupport(physical_device, surface);

  VkSurfaceFormatKHR surface_format =
      ChooseSwapSurfaceFormat(swap_chain_support.formats);
  VkPresentModeKHR present_mode =
      ChooseSwapPresentMode(swap_chain_support.present_modes);
  VkExtent2D extent = ChooseSwapExtent(swap_chain_support.capabilities, window);

  // https://vulkan-tutorial.com/Drawing_a_triangle/Presentation/Swap_chain
  // recommends requesting 1 more image than the minimum to avoid waiting
  // on the driver to complete "internal operations".
  uint32_t image_count = swap_chain_support.capabilities.minImageCount + 1;
  if (swap_chain_support.capabilities.maxImageCount > 0 &&
      image_count > swap_chain_support.capabilities.maxImageCount) {
    image_count = swap_chain_support.capabilities.maxImageCount;
  }

  uint32_t present_queue_family_index =
      *FindFirstPresentQueueFamilyIndex(physical_device, surface);
  uint32_t queue_family_indices[] = {graphics_queue_family_index,
                                     present_queue_family_index};

  VkSwapchainCreateInfoKHR swap_chain_create_info{};
  swap_chain_create_info.sType = VK_STRUCTURE_TYPE_SWAPCHAIN_CREATE_INFO_KHR;
  swap_chain_create_info.surface = surface;
  swap_chain_create_info.minImageCount = image_count;
  swap_chain_create_info.imageFormat = surface_format.format;
  swap_chain_create_info.imageColorSpace = surface_format.colorSpace;
  swap_chain_create_info.imageExtent = extent;
  swap_chain_create_info.imageArrayLayers = 1;
  swap_chain_create_info.imageUsage = VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT;
  if (graphics_queue_family_index != present_queue_family_index) {
    // According to
    // https://vulkan-tutorial.com/Drawing_a_triangle/Presentation/Window_surface,
    // this isn't the most efficient setup in the case that the graphics queue
    // is separate from the present queue, however it's also apparently an
    // uncommon situation.
    swap_chain_create_info.imageSharingMode = VK_SHARING_MODE_CONCURRENT;
    swap_chain_create_info.queueFamilyIndexCount = 2;
    swap_chain_create_info.pQueueFamilyIndices = queue_family_indices;
  } else {
    swap_chain_create_info.imageSharingMode = VK_SHARING_MODE_EXCLUSIVE;
    swap_chain_create_info.queueFamilyIndexCount = 0;      // Optional
    swap_chain_create_info.pQueueFamilyIndices = nullptr;  // Optional
  }
  swap_chain_create_info.preTransform =
      swap_chain_support.capabilities.currentTransform;
  swap_chain_create_info.compositeAlpha = VK_COMPOSITE_ALPHA_OPAQUE_BIT_KHR;
  swap_chain_create_info.presentMode = present_mode;
  swap_chain_create_info.clipped = VK_TRUE;
  swap_chain_create_info.oldSwapchain =
      VK_NULL_HANDLE;  // TODO: Handle resetting swap chains.

  VkSwapchainKHR swap_chain;
  result = vkCreateSwapchainKHR(device, &swap_chain_create_info, nullptr,
                                &swap_chain);
  if (result != VK_SUCCESS) {
    std::cerr << "Error creating Vulkan swap chain." << std::endl;
    return 1;
  }

  vkGetSwapchainImagesKHR(device, swap_chain, &image_count, nullptr);
  std::vector<VkImage> swap_chain_images;
  swap_chain_images.resize(image_count);
  vkGetSwapchainImagesKHR(device, swap_chain, &image_count,
                          swap_chain_images.data());

  // Setup the image viewsw.
  std::vector<VkImageView> swap_chain_image_views;
  swap_chain_image_views.resize(swap_chain_images.size());
  for (size_t i = 0; i < swap_chain_images.size(); i++) {
    VkImageViewCreateInfo image_view_create_info{};
    image_view_create_info.sType = VK_STRUCTURE_TYPE_IMAGE_VIEW_CREATE_INFO;
    image_view_create_info.image = swap_chain_images[i];
    image_view_create_info.viewType = VK_IMAGE_VIEW_TYPE_2D;
    image_view_create_info.format = surface_format.format;
    image_view_create_info.components.r = VK_COMPONENT_SWIZZLE_IDENTITY;
    image_view_create_info.components.g = VK_COMPONENT_SWIZZLE_IDENTITY;
    image_view_create_info.components.b = VK_COMPONENT_SWIZZLE_IDENTITY;
    image_view_create_info.components.a = VK_COMPONENT_SWIZZLE_IDENTITY;
    image_view_create_info.subresourceRange.aspectMask =
        VK_IMAGE_ASPECT_COLOR_BIT;
    image_view_create_info.subresourceRange.baseMipLevel = 0;
    image_view_create_info.subresourceRange.levelCount = 1;
    image_view_create_info.subresourceRange.baseArrayLayer = 0;
    image_view_create_info.subresourceRange.layerCount = 1;
    result = vkCreateImageView(device, &image_view_create_info, nullptr,
                               &swap_chain_image_views[i]);
    if (result != VK_SUCCESS) {
      std::cerr << "Error creating Vulkan image view." << std::endl;
      return 1;
    }
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

  for (auto image_view : swap_chain_image_views) {
    vkDestroyImageView(device, image_view, nullptr);
  }
  vkDestroySwapchainKHR(device, swap_chain, nullptr);
  vkDestroySurfaceKHR(vulkan_instance, surface, nullptr);
  PlatformWindowDestroyWindow(window);
  vkDestroyDevice(device, nullptr);
  vkDestroyInstance(vulkan_instance, nullptr);

  return 0;
}

}  // namespace typescript_cpp_v8
}  // namespace reify
