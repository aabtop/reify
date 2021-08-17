#ifndef _REIFY_SRC_VULKAN_UTILS_INCLUDE_VULKAN_UTILS_SIMPLE_RENDER_PASS_RENDERER_H_
#define _REIFY_SRC_VULKAN_UTILS_INCLUDE_VULKAN_UTILS_SIMPLE_RENDER_PASS_RENDERER_H_

#include <memory>

#include "vulkan_utils/vulkan_utils.h"

namespace vulkan_utils {

class SimpleRenderPassRenderer {
 public:
  using FrameResources = std::any;

  static vulkan_utils::ErrorOr<SimpleRenderPassRenderer> Create(
      VkInstance instance, VkPhysicalDevice physical_device, VkDevice device,
      VkFormat output_image_format);

  vulkan_utils::ErrorOr<FrameResources> Render(
      VkCommandBuffer command_buffer, VkFramebuffer framebuffer,
      VkImage output_color_image, int viewport_left, int viewport_top,
      int viewport_right, int viewport_bottom,
      const std::function<vulkan_utils::ErrorOr<FrameResources>(
          VkCommandBuffer)>& render_with_render_pass);

  VkRenderPass render_pass() const { return data_.render_pass->value(); }

 private:
  struct Data {
    VkInstance instance;
    VkPhysicalDevice physical_device;
    VkDevice device;
    std::shared_ptr<vulkan_utils::WithDeleter<VkRenderPass>> render_pass;
  };
  SimpleRenderPassRenderer(Data&& data) : data_(std::move(data)) {}

  Data data_;
};

}  // namespace vulkan_utils

#endif  // _REIFY_SRC_VULKAN_UTILS_INCLUDE_VULKAN_UTILS_SIMPLE_RENDER_PASS_RENDERER_H_
