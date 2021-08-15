#ifndef _HYPO_SRC_VISUALIZER_VULKAN_SIMPLE_RENDER_PASS_RENDERER_H
#define _HYPO_SRC_VISUALIZER_VULKAN_SIMPLE_RENDER_PASS_RENDERER_H

#include "reify/window/window.h"
#include "vulkan_utils/vulkan_utils.h"

namespace hypo {
namespace visualizer {
namespace vulkan {

class SimpleRenderPassRenderer {
 public:
  static vulkan_utils::ErrorOr<SimpleRenderPassRenderer> Create(
      VkInstance instance, VkPhysicalDevice physical_device, VkDevice device,
      VkFormat output_image_format);

  vulkan_utils::ErrorOr<reify::window::Window::Renderer::FrameResources> Render(
      VkCommandBuffer command_buffer, VkFramebuffer framebuffer,
      VkImage output_color_image, const reify::window::Rect& viewport_region,
      const std::function<vulkan_utils::ErrorOr<
          reify::window::Window::Renderer::FrameResources>(VkCommandBuffer)>&
          render_with_render_pass);

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

}  // namespace vulkan
}  // namespace visualizer
}  // namespace hypo

#endif  // _HYPO_SRC_VISUALIZER_VULKAN_SIMPLE_RENDER_PASS_RENDERER_H
