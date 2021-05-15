#include "reify/typescript_cpp_v8/visualizer_window.h"

#include <vulkan/vulkan.h>

#include <condition_variable>
#include <iostream>
#include <mutex>
#include <optional>

#include "platform_window/platform_window.h"
#include "platform_window/vulkan.h"
#include "vulkan_utils/vulkan_utils.h"

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
  VkExtent2D swap_chain_extent =
      ChooseSwapExtent(swap_chain_support.capabilities, window);

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
  swap_chain_create_info.imageExtent = swap_chain_extent;
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

  vulkan_utils::WithDeleter<VkRenderPass> render_pass = std::get<
      1>(vulkan_utils::MakeRenderPass(
      device,
      {VkAttachmentDescription{
          0, surface_format.format, VK_SAMPLE_COUNT_1_BIT,
          VK_ATTACHMENT_LOAD_OP_CLEAR, VK_ATTACHMENT_STORE_OP_STORE,
          VK_ATTACHMENT_LOAD_OP_DONT_CARE, VK_ATTACHMENT_STORE_OP_DONT_CARE,
          VK_IMAGE_LAYOUT_UNDEFINED,
          VK_IMAGE_LAYOUT_PRESENT_SRC_KHR,  // VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL?
      }},
      VkAttachmentDescription{
          0, VK_FORMAT_D32_SFLOAT, VK_SAMPLE_COUNT_1_BIT,
          VK_ATTACHMENT_LOAD_OP_CLEAR, VK_ATTACHMENT_STORE_OP_DONT_CARE,
          VK_ATTACHMENT_LOAD_OP_DONT_CARE, VK_ATTACHMENT_STORE_OP_DONT_CARE,
          VK_IMAGE_LAYOUT_UNDEFINED,
          VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL,  // VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL?
      }));

  // Create the depth buffer.
  vulkan_utils::WithDeleter<VkImage> depth_image =
      std::get<1>(vulkan_utils::MakeImage(
          device, swap_chain_extent.width, swap_chain_extent.height,
          VK_FORMAT_D32_SFLOAT, VK_IMAGE_TILING_OPTIMAL,
          VK_IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT));
  vulkan_utils::WithDeleter<VkDeviceMemory> depth_image_memory =
      std::get<1>(vulkan_utils::AllocateAndBindImageMemory(
          physical_device, device, depth_image.value(),
          VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT));
  vulkan_utils::WithDeleter<VkImageView> depth_image_view =
      std::get<1>(vulkan_utils::MakeImageView(device, depth_image.value(),
                                              VK_FORMAT_D32_SFLOAT));

  // Create the framebuffers.
  std::vector<VkFramebuffer> swap_chain_framebuffers;
  swap_chain_framebuffers.resize(swap_chain_image_views.size());
  for (size_t i = 0; i < swap_chain_image_views.size(); i++) {
    std::array<VkImageView, 2> attachments = {swap_chain_image_views[i],
                                              depth_image_view.value()};

    VkFramebufferCreateInfo framebuffer_info{};
    framebuffer_info.sType = VK_STRUCTURE_TYPE_FRAMEBUFFER_CREATE_INFO;
    framebuffer_info.renderPass = render_pass.value();
    framebuffer_info.attachmentCount =
        static_cast<uint32_t>(attachments.size());
    framebuffer_info.pAttachments = attachments.data();
    framebuffer_info.width = swap_chain_extent.width;
    framebuffer_info.height = swap_chain_extent.height;
    framebuffer_info.layers = 1;

    result = vkCreateFramebuffer(device, &framebuffer_info, nullptr,
                                 &swap_chain_framebuffers[i]);
    if (result != VK_SUCCESS) {
      std::cerr << "Failed to create framebuffer!" << std::endl;
      return 1;
    }
  }

  VkCommandPoolCreateInfo command_pool_info{};
  command_pool_info.sType = VK_STRUCTURE_TYPE_COMMAND_POOL_CREATE_INFO;
  command_pool_info.queueFamilyIndex = graphics_queue_family_index;
  command_pool_info.flags = VK_COMMAND_POOL_CREATE_TRANSIENT_BIT;

  VkCommandPool command_pool;
  result =
      vkCreateCommandPool(device, &command_pool_info, nullptr, &command_pool);
  if (result != VK_SUCCESS) {
    std::cerr << "Failed to create command pool." << std::endl;
    return 1;
  }

  std::vector<VkCommandBuffer> command_buffers;
  command_buffers.resize(swap_chain_framebuffers.size());
  VkCommandBufferAllocateInfo command_buffer_alloc_info{};
  command_buffer_alloc_info.sType =
      VK_STRUCTURE_TYPE_COMMAND_BUFFER_ALLOCATE_INFO;
  command_buffer_alloc_info.commandPool = command_pool;
  command_buffer_alloc_info.level = VK_COMMAND_BUFFER_LEVEL_PRIMARY;
  command_buffer_alloc_info.commandBufferCount =
      static_cast<uint32_t>(command_buffers.size());

  result = vkAllocateCommandBuffers(device, &command_buffer_alloc_info,
                                    command_buffers.data());
  if (result != VK_SUCCESS) {
    std::cerr << "Failed to allocate command buffers!" << std::endl;
    return 1;
  }

  vulkan_utils::WithDeleter<VkSemaphore> image_available_semaphore =
      std::get<1>(vulkan_utils::MakeSemaphore(device));
  vulkan_utils::WithDeleter<VkSemaphore> render_finished_semaphore =
      std::get<1>(vulkan_utils::MakeSemaphore(device));

  // Prepare to spin up the main loop.
  bool window_visible = false;
  PlatformWindowEnqueueCustomEvent(window, {});
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
      case kPlatformWindowEventTypeCustom: {
        uint32_t current_swap_chain_index;
        vkAcquireNextImageKHR(device, swap_chain, UINT64_MAX,
                              image_available_semaphore.value(), VK_NULL_HANDLE,
                              &current_swap_chain_index);

        VkCommandBuffer current_command_buffer =
            command_buffers[current_swap_chain_index];
        VkFramebuffer current_framebuffer =
            swap_chain_framebuffers[current_swap_chain_index];

        VkCommandBufferBeginInfo command_buffer_begin_info{};
        command_buffer_begin_info.sType =
            VK_STRUCTURE_TYPE_COMMAND_BUFFER_BEGIN_INFO;
        command_buffer_begin_info.flags =
            VK_COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT;
        command_buffer_begin_info.pInheritanceInfo = nullptr;

        result = vkBeginCommandBuffer(current_command_buffer,
                                      &command_buffer_begin_info);
        if (result != VK_SUCCESS) {
          std::cerr << "Error starting new command buffer." << std::endl;
          return 1;
        }

        // Draw some cool shit.
        std::array<VkClearValue, 2> clear_values{};
        memset(clear_values.data(), 0,
               sizeof(clear_values[0]) * clear_values.size());
        clear_values[0].color = {{0.11, 0.11, 0.11, 1}};
        clear_values[1].depthStencil = {1, 0};

        VkRenderPassBeginInfo rpb{};
        rpb.sType = VK_STRUCTURE_TYPE_RENDER_PASS_BEGIN_INFO;
        rpb.renderPass = render_pass.value();
        rpb.framebuffer = current_framebuffer;
        rpb.renderArea.extent.width = swap_chain_extent.width;
        rpb.renderArea.extent.height = swap_chain_extent.height;
        rpb.clearValueCount = clear_values.size();
        rpb.pClearValues = clear_values.data();
        vkCmdBeginRenderPass(current_command_buffer, &rpb,
                             VK_SUBPASS_CONTENTS_INLINE);
        vkCmdEndRenderPass(current_command_buffer);

        result = vkEndCommandBuffer(current_command_buffer);
        if (result != VK_SUCCESS) {
          std::cerr << "Failed to record command buffer!" << std::endl;
          return 1;
        }

        VkSubmitInfo submit_info{};
        submit_info.sType = VK_STRUCTURE_TYPE_SUBMIT_INFO;

        VkSemaphore wait_semaphores[] = {image_available_semaphore.value()};
        VkPipelineStageFlags wait_stages[] = {
            VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT};
        submit_info.waitSemaphoreCount = 1;
        submit_info.pWaitSemaphores = wait_semaphores;
        submit_info.pWaitDstStageMask = wait_stages;
        submit_info.commandBufferCount = 1;
        submit_info.pCommandBuffers = &current_command_buffer;
        VkSemaphore signal_semaphores[] = {render_finished_semaphore.value()};
        submit_info.signalSemaphoreCount = 1;
        submit_info.pSignalSemaphores = signal_semaphores;

        VkQueue graphics_queue;
        vkGetDeviceQueue(device, graphics_queue_family_index, 0,
                         &graphics_queue);
        result = vkQueueWaitIdle(graphics_queue);
        if (result != VK_SUCCESS) {
          std::cerr << "Error waiting for graphics queue to become idle."
                    << std::endl;
          return 1;
        }
        result = vkQueueSubmit(graphics_queue, 1, &submit_info, VK_NULL_HANDLE);
        if (result != VK_SUCCESS) {
          std::cerr << "Error submitting Vulkan command queue." << std::endl;
          return 1;
        }

        VkQueue present_queue;
        vkGetDeviceQueue(device, present_queue_family_index, 0, &present_queue);
        VkPresentInfoKHR present_info{};
        present_info.sType = VK_STRUCTURE_TYPE_PRESENT_INFO_KHR;
        present_info.waitSemaphoreCount = 1;
        present_info.pWaitSemaphores = signal_semaphores;
        VkSwapchainKHR swap_chains[] = {swap_chain};
        present_info.swapchainCount = 1;
        present_info.pSwapchains = swap_chains;
        present_info.pImageIndices = &current_swap_chain_index;
        present_info.pResults = nullptr;
        vkQueuePresentKHR(present_queue, &present_info);

        if (!window_visible) {
          result = vkQueueWaitIdle(present_queue);
          if (result != VK_SUCCESS) {
            std::cerr << "Error waiting for graphics queue to become idle."
                      << std::endl;
            return 1;
          }

          window_visible = true;
          PlatformWindowShow(window);
        }
        PlatformWindowEnqueueCustomEvent(window, {});
      } break;
    }

    if (should_quit) {
      break;
    }
  }

  // Before cleaning up resources, wait until the device is idle.
  vkDeviceWaitIdle(device);

  vkDestroyCommandPool(device, command_pool, nullptr);
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
