#include "simple_render_pass_renderer.h"

#include "vulkan_utils/vulkan_utils.h"

namespace hypo {
namespace visualizer {
namespace vulkan {

vulkan_utils::ErrorOr<SimpleRenderPassRenderer>
SimpleRenderPassRenderer::Create(VkInstance instance,
                                 VkPhysicalDevice physical_device,
                                 VkDevice device,
                                 VkFormat output_image_format) {
  VULKAN_UTILS_ASSIGN_OR_RETURN(
      render_pass, vulkan_utils::MakeRenderPass(
                       device,
                       {VkAttachmentDescription{
                           0,
                           output_image_format,
                           VK_SAMPLE_COUNT_1_BIT,
                           VK_ATTACHMENT_LOAD_OP_CLEAR,
                           VK_ATTACHMENT_STORE_OP_STORE,
                           VK_ATTACHMENT_LOAD_OP_DONT_CARE,
                           VK_ATTACHMENT_STORE_OP_DONT_CARE,
                           VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL,
                           VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL,
                       }},
                       VkAttachmentDescription{
                           0,
                           VK_FORMAT_D32_SFLOAT,
                           VK_SAMPLE_COUNT_1_BIT,
                           VK_ATTACHMENT_LOAD_OP_CLEAR,
                           VK_ATTACHMENT_STORE_OP_DONT_CARE,
                           VK_ATTACHMENT_LOAD_OP_DONT_CARE,
                           VK_ATTACHMENT_STORE_OP_DONT_CARE,
                           VK_IMAGE_LAYOUT_UNDEFINED,
                           VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL,
                       }));
  return SimpleRenderPassRenderer(SimpleRenderPassRenderer::Data{
      instance, physical_device, device,
      std::make_shared<vulkan_utils::WithDeleter<VkRenderPass>>(
          std::move(render_pass))});
}

vulkan_utils::ErrorOr<reify::window::Window::Renderer::FrameResources>
SimpleRenderPassRenderer::Render(
    VkCommandBuffer command_buffer, VkFramebuffer framebuffer,
    VkImage output_color_image, const reify::window::Rect& viewport_region,
    const std::function<
        vulkan_utils::ErrorOr<reify::window::Window::Renderer::FrameResources>(
            VkCommandBuffer)>& render_with_render_pass) {
  VkViewport viewport;
  viewport.x = viewport_region.left;
  viewport.y = viewport_region.top;
  viewport.width = viewport_region.right - viewport_region.left;
  viewport.height = viewport_region.bottom - viewport_region.top;
  viewport.minDepth = 0;
  viewport.maxDepth = 1;
  vkCmdSetViewport(command_buffer, 0, 1, &viewport);

  VkRect2D scissor;
  scissor.offset.x = viewport_region.left;
  scissor.offset.y = viewport_region.top;
  scissor.extent.width = viewport_region.right - viewport_region.left;
  scissor.extent.height = viewport_region.bottom - viewport_region.top;
  vkCmdSetScissor(command_buffer, 0, 1, &scissor);

  std::array<VkClearValue, 2> clear_values{};
  memset(clear_values.data(), 0, sizeof(clear_values[0]) * clear_values.size());
  clear_values[0].color = {{0.11, 0.11, 0.11, 1}};
  clear_values[1].depthStencil = {1, 0};

  VkRenderPassBeginInfo rpb{};
  rpb.sType = VK_STRUCTURE_TYPE_RENDER_PASS_BEGIN_INFO;
  rpb.renderPass = data_.render_pass->value();
  rpb.framebuffer = framebuffer;
  rpb.renderArea.offset.x = viewport_region.left;
  rpb.renderArea.offset.y = viewport_region.top;
  rpb.renderArea.extent.width = viewport_region.right - viewport_region.left;
  rpb.renderArea.extent.height = viewport_region.bottom - viewport_region.top;
  rpb.clearValueCount = clear_values.size();
  rpb.pClearValues = clear_values.data();

  vkCmdBeginRenderPass(command_buffer, &rpb, VK_SUBPASS_CONTENTS_INLINE);

  auto error_or_resources = render_with_render_pass(command_buffer);
  if (auto error = std::get_if<0>(&error_or_resources)) {
    return *error;
  }

  vkCmdEndRenderPass(command_buffer);

  // std::any doesn't support move only types, so we wrap it in a shared_ptr.
  return reify::window::Window::Renderer::FrameResources(
      std::make_tuple(std::get<1>(error_or_resources), data_.render_pass));
}

}  // namespace vulkan
}  // namespace visualizer
}  // namespace hypo