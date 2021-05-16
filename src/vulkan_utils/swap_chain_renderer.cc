#include "vulkan_utils/swap_chain_renderer.h"

#include <fmt/format.h>
#include <vulkan/vulkan.h>

#include <algorithm>
#include <array>
#include <cassert>
#include <iostream>

#include "vulkan_utils/vulkan_utils.h"

namespace vulkan_utils {

namespace {
VkPhysicalDevice PickBestPhysicalDevice(
    const std::vector<VkPhysicalDevice>& physical_device) {
  assert(!physical_device.empty());
  return physical_device[0];
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

ErrorOr<SwapChainSupportDetails> QuerySwapChainSupport(
    VkPhysicalDevice physical_device, VkSurfaceKHR surface) {
  VkResult err;
  SwapChainSupportDetails details;

  err = vkGetPhysicalDeviceSurfaceCapabilitiesKHR(physical_device, surface,
                                                  &details.capabilities);
  if (err != VK_SUCCESS) {
    return Error{fmt::format(
        "Error getting physical device surface capabilities: {}", err)};
  }

  uint32_t format_count;
  err = vkGetPhysicalDeviceSurfaceFormatsKHR(physical_device, surface,
                                             &format_count, nullptr);
  if (err != VK_SUCCESS) {
    return Error{fmt::format(
        "Error getting physical device surface format count: {}", err)};
  }

  if (format_count == 0) {
    return Error{"No physical device surface formats found."};
  }

  details.formats.resize(format_count);
  err = vkGetPhysicalDeviceSurfaceFormatsKHR(
      physical_device, surface, &format_count, details.formats.data());
  if (err != VK_SUCCESS) {
    return Error{
        fmt::format("Error getting physical device surface formats: {}", err)};
  }

  uint32_t present_mode_count;
  err = vkGetPhysicalDeviceSurfacePresentModesKHR(physical_device, surface,
                                                  &present_mode_count, nullptr);
  if (err != VK_SUCCESS) {
    return Error{fmt::format(
        "Error getting phsyical device surface presentation mode count: {}",
        err)};
  }

  if (present_mode_count == 0) {
    return Error{
        fmt::format("No physical device surface presentation modes found.")};
  }

  details.present_modes.resize(present_mode_count);
  err = vkGetPhysicalDeviceSurfacePresentModesKHR(physical_device, surface,
                                                  &present_mode_count,
                                                  details.present_modes.data());
  if (err != VK_SUCCESS) {
    return Error{fmt::format(
        "Error getting physical device surface presentation modes: {}", err)};
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
                            int32_t width, int32_t height) {
  if (capabilities.currentExtent.width != UINT32_MAX) {
    return capabilities.currentExtent;
  } else {
    std::cerr << "Not-well-tested-code alert: Pixels != screen coordinates."
              << std::endl;

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

ErrorOr<SwapChainRenderer> SwapChainRenderer::Create(VkInstance instance,
                                                     VkSurfaceKHR surface,
                                                     int32_t width,
                                                     int32_t height) {
  VkResult err;

  VULKAN_UTILS_ASSIGN_OR_RETURN(physical_devices,
                                EnumeratePhysicalDevices(instance));

  VkPhysicalDevice physical_device = PickBestPhysicalDevice(physical_devices);

  std::optional<uint32_t> maybe_graphics_queue_family_index =
      FindFirstQueueFamilyIndex(physical_device, VK_QUEUE_GRAPHICS_BIT);
  if (!maybe_graphics_queue_family_index) {
    return Error{"Could not find any Vulkan graphics queues."};
  }

  std::optional<uint32_t> maybe_present_queue_family_index =
      FindFirstPresentQueueFamilyIndex(physical_device, surface);
  if (!maybe_present_queue_family_index) {
    return Error{"Could not find any Vulkan presentation queues."};
  }

  VULKAN_UTILS_ASSIGN_OR_RETURN(
      device, MakeDevice(physical_device, *maybe_graphics_queue_family_index,
                         {VK_KHR_SWAPCHAIN_EXTENSION_NAME}));

  // Create the swapchain.
  VULKAN_UTILS_ASSIGN_OR_RETURN(
      swap_chain_support, QuerySwapChainSupport(physical_device, surface));

  VkSurfaceFormatKHR surface_format =
      ChooseSwapSurfaceFormat(swap_chain_support.formats);
  VkPresentModeKHR present_mode =
      ChooseSwapPresentMode(swap_chain_support.present_modes);
  VkExtent2D swap_chain_extent =
      ChooseSwapExtent(swap_chain_support.capabilities, width, height);

  // https://vulkan-tutorial.com/Drawing_a_triangle/Presentation/Swap_chain
  // recommends requesting 1 more image than the minimum to avoid waiting
  // on the driver to complete "internal operations".
  uint32_t image_count = swap_chain_support.capabilities.minImageCount + 1;
  if (swap_chain_support.capabilities.maxImageCount > 0 &&
      image_count > swap_chain_support.capabilities.maxImageCount) {
    image_count = swap_chain_support.capabilities.maxImageCount;
  }

  VULKAN_UTILS_ASSIGN_OR_RETURN(
      swap_chain,
      MakeSwapChain(device.value(), surface, image_count, surface_format,
                    present_mode, swap_chain_extent,
                    swap_chain_support.capabilities.currentTransform,
                    *maybe_graphics_queue_family_index,
                    *maybe_present_queue_family_index));

  err = vkGetSwapchainImagesKHR(device.value(), swap_chain.value(),
                                &image_count, nullptr);
  if (err != VK_SUCCESS) {
    return Error{fmt::format("Error getting swapchain images count: ", err)};
  }
  std::vector<VkImage> swap_chain_images;
  swap_chain_images.resize(image_count);
  err = vkGetSwapchainImagesKHR(device.value(), swap_chain.value(),
                                &image_count, swap_chain_images.data());
  if (err != VK_SUCCESS) {
    return Error{fmt::format("Error getting swapchain images: ", err)};
  }

  // Setup the image views.
  std::vector<WithDeleter<VkImageView>> swap_chain_image_views;
  swap_chain_image_views.reserve(swap_chain_images.size());
  for (const auto& swap_chain_image : swap_chain_images) {
    VULKAN_UTILS_ASSIGN_OR_RETURN(
        swap_chain_image_view,
        MakeImageView(device.value(), swap_chain_image, surface_format.format));
    swap_chain_image_views.push_back(std::move(swap_chain_image_view));
  }

  VULKAN_UTILS_ASSIGN_OR_RETURN(
      render_pass,
      MakeRenderPass(
          device.value(),
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
  VULKAN_UTILS_ASSIGN_OR_RETURN(
      depth_image, MakeImage(device.value(), swap_chain_extent.width,
                             swap_chain_extent.height, VK_FORMAT_D32_SFLOAT,
                             VK_IMAGE_TILING_OPTIMAL,
                             VK_IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT));
  VULKAN_UTILS_ASSIGN_OR_RETURN(
      depth_image_memory,
      AllocateAndBindImageMemory(physical_device, device.value(),
                                 depth_image.value(),
                                 VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT));
  VULKAN_UTILS_ASSIGN_OR_RETURN(
      depth_image_view,
      MakeImageView(device.value(), depth_image.value(), VK_FORMAT_D32_SFLOAT));

  // Create the framebuffers.
  std::vector<WithDeleter<VkFramebuffer>> swap_chain_framebuffers;
  swap_chain_framebuffers.reserve(swap_chain_image_views.size());
  for (const auto& swap_chain_image_view : swap_chain_image_views) {
    VULKAN_UTILS_ASSIGN_OR_RETURN(
        framebuffer,
        MakeFramebuffer(device.value(), render_pass.value(),
                        {swap_chain_image_view.value()},
                        depth_image_view.value(), swap_chain_extent));
    swap_chain_framebuffers.push_back(std::move(framebuffer));
  }

  VULKAN_UTILS_ASSIGN_OR_RETURN(
      command_pool,
      MakeCommandPool(device.value(), *maybe_graphics_queue_family_index));

  VULKAN_UTILS_ASSIGN_OR_RETURN(
      command_buffers, MakeCommandBuffers(device.value(), command_pool.value(),
                                          swap_chain_framebuffers.size()));

  WithDeleter<VkSemaphore> image_available_semaphore =
      std::get<1>(MakeSemaphore(device.value()));
  WithDeleter<VkSemaphore> render_finished_semaphore =
      std::get<1>(MakeSemaphore(device.value()));

  return SwapChainRenderer(
      instance, surface, physical_device, *maybe_graphics_queue_family_index,
      *maybe_present_queue_family_index, std::move(device), swap_chain_extent,
      std::move(swap_chain), std::move(swap_chain_image_views),
      std::move(render_pass), std::move(depth_image),
      std::move(depth_image_memory), std::move(depth_image_view),
      std::move(swap_chain_framebuffers), std::move(command_pool),
      command_buffers, std::move(image_available_semaphore),
      std::move(render_finished_semaphore));
}

std::optional<Error> SwapChainRenderer::Render(
    const std::function<ErrorOr<FrameResources>(
        VkCommandBuffer command_buffer, VkFramebuffer framebuffer,
        const std::array<uint32_t, 2>& output_surface_size)>& render_function) {
  VkResult err;
  uint32_t current_swap_chain_index;
  err = vkAcquireNextImageKHR(device_.value(), swap_chain_.value(), UINT64_MAX,
                              image_available_semaphore_.value(),
                              VK_NULL_HANDLE, &current_swap_chain_index);
  if (err != VK_SUCCESS) {
    return Error{fmt::format(
        "Error acquiring next swap chain image for rendering: {}", err)};
  }

  VkCommandBuffer current_command_buffer =
      command_buffers_[current_swap_chain_index];
  VkFramebuffer current_framebuffer =
      swap_chain_framebuffers_[current_swap_chain_index].value();

  VkCommandBufferBeginInfo command_buffer_begin_info{};
  command_buffer_begin_info.sType = VK_STRUCTURE_TYPE_COMMAND_BUFFER_BEGIN_INFO;
  command_buffer_begin_info.flags = VK_COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT;
  command_buffer_begin_info.pInheritanceInfo = nullptr;

  err =
      vkBeginCommandBuffer(current_command_buffer, &command_buffer_begin_info);
  if (err != VK_SUCCESS) {
    return Error{fmt::format("Error starting new command buffer: {}", err)};
  }

  // Draw some cool shit.
  std::array<VkClearValue, 2> clear_values{};
  memset(clear_values.data(), 0, sizeof(clear_values[0]) * clear_values.size());
  clear_values[0].color = {{0.11, 0.11, 0.11, 1}};
  clear_values[1].depthStencil = {1, 0};

  auto frame_resources =
      render_function(current_command_buffer, current_framebuffer,
                      {swap_chain_extent_.width, swap_chain_extent_.height});

  VkRenderPassBeginInfo rpb{};
  rpb.sType = VK_STRUCTURE_TYPE_RENDER_PASS_BEGIN_INFO;
  rpb.renderPass = render_pass_.value();
  rpb.framebuffer = current_framebuffer;
  rpb.renderArea.extent.width = swap_chain_extent_.width;
  rpb.renderArea.extent.height = swap_chain_extent_.height;
  rpb.clearValueCount = clear_values.size();
  rpb.pClearValues = clear_values.data();
  vkCmdBeginRenderPass(current_command_buffer, &rpb,
                       VK_SUBPASS_CONTENTS_INLINE);
  vkCmdEndRenderPass(current_command_buffer);

  err = vkEndCommandBuffer(current_command_buffer);
  if (err != VK_SUCCESS) {
    return Error{fmt::format("Failed to record command buffer: {}", err)};
  }

  VkSubmitInfo submit_info{};
  submit_info.sType = VK_STRUCTURE_TYPE_SUBMIT_INFO;

  VkSemaphore wait_semaphores[] = {image_available_semaphore_.value()};
  VkPipelineStageFlags wait_stages[] = {
      VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT};
  submit_info.waitSemaphoreCount = 1;
  submit_info.pWaitSemaphores = wait_semaphores;
  submit_info.pWaitDstStageMask = wait_stages;
  submit_info.commandBufferCount = 1;
  submit_info.pCommandBuffers = &current_command_buffer;
  VkSemaphore signal_semaphores[] = {render_finished_semaphore_.value()};
  submit_info.signalSemaphoreCount = 1;
  submit_info.pSignalSemaphores = signal_semaphores;

  VkQueue graphics_queue;
  vkGetDeviceQueue(device_.value(), graphics_queue_family_index_, 0,
                   &graphics_queue);
  err = vkQueueWaitIdle(graphics_queue);
  if (err != VK_SUCCESS) {
    return Error{fmt::format(
        "Error waiting for graphics queue to become idle: {}", err)};
  }
  err = vkQueueSubmit(graphics_queue, 1, &submit_info, VK_NULL_HANDLE);
  if (err != VK_SUCCESS) {
    return Error{fmt::format("Error submitting Vulkan command queue: {}", err)};
  }

  VkQueue present_queue;
  vkGetDeviceQueue(device_.value(), present_queue_family_index_, 0,
                   &present_queue);
  VkPresentInfoKHR present_info{};
  present_info.sType = VK_STRUCTURE_TYPE_PRESENT_INFO_KHR;
  present_info.waitSemaphoreCount = 1;
  present_info.pWaitSemaphores = signal_semaphores;
  VkSwapchainKHR swap_chains[] = {swap_chain_.value()};
  present_info.swapchainCount = 1;
  present_info.pSwapchains = swap_chains;
  present_info.pImageIndices = &current_swap_chain_index;
  present_info.pResults = nullptr;
  err = vkQueuePresentKHR(present_queue, &present_info);
  if (err != VK_SUCCESS) {
    return Error{fmt::format(
        "Error adding present command to Vulkan present queue: {}", err)};
  }

  if (!has_rendered_) {
    err = vkQueueWaitIdle(present_queue);
    if (err != VK_SUCCESS) {
      return Error{fmt::format(
          "Error waiting for graphics queue to become idle: {}", err)};
    }

    has_rendered_ = true;
  }

  return std::nullopt;
}

SwapChainRenderer::~SwapChainRenderer() {
  // Before cleaning up resources, wait until the device is idle.
  vkDeviceWaitIdle(device_.value());
}

}  // namespace vulkan_utils
