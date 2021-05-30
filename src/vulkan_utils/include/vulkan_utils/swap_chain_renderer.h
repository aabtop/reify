#ifndef _REIFY_SRC_VULKAN_UTILS_INCLUDE_VULKAN_UTILS_SWAP_CHAIN_RENDERER_H_
#define _REIFY_SRC_VULKAN_UTILS_INCLUDE_VULKAN_UTILS_SWAP_CHAIN_RENDERER_H_

#include "vulkan_utils/vulkan_utils.h"

namespace vulkan_utils {

class SwapChainRenderer {
 public:
  static ErrorOr<SwapChainRenderer> Create(VkInstance instance,
                                           VkSurfaceKHR surface, int32_t width,
                                           int32_t height);
  SwapChainRenderer(SwapChainRenderer&&) = default;

  ~SwapChainRenderer();

  const VkPhysicalDevice physical_device() const { return physical_device_; }
  const VkDevice device() const { return device_.value(); }
  const VkSurfaceFormatKHR& surface_format() const { return surface_format_; }

  std::optional<Error> Render(
      const std::function<ErrorOr<FrameResources>(
          VkCommandBuffer command_buffer, VkFramebuffer framebuffer,
          VkImage output_color_image,
          const std::array<uint32_t, 2>& output_surface_size)>&
          render_function);

 private:
  SwapChainRenderer(
      VkInstance& instance, VkSurfaceKHR& surface,
      VkPhysicalDevice& physical_device, uint32_t graphics_queue_family_index,
      uint32_t present_queue_family_index, WithDeleter<VkDevice>&& device,
      const VkSurfaceFormatKHR& surface_format,
      const VkExtent2D& swap_chain_extent,
      WithDeleter<VkSwapchainKHR>&& swap_chain,
      std::vector<VkImage>&& swap_chain_images,
      std::vector<WithDeleter<VkImageView>>&& swap_chain_image_views,
      WithDeleter<VkRenderPass>&& render_pass,
      WithDeleter<VkImage>&& depth_image,
      WithDeleter<VkDeviceMemory>&& depth_image_memory,
      WithDeleter<VkImageView>&& depth_image_view,
      std::vector<WithDeleter<VkFramebuffer>>&& swap_chain_framebuffers,
      WithDeleter<VkCommandPool>&& command_pool,
      const std::vector<VkCommandBuffer>& command_buffers,
      WithDeleter<VkSemaphore>&& image_available_semaphore,
      WithDeleter<VkSemaphore>&& render_finished_semaphore)
      : instance_(instance),
        surface_(surface),
        physical_device_(physical_device),
        graphics_queue_family_index_(graphics_queue_family_index),
        present_queue_family_index_(present_queue_family_index),
        device_(std::move(device)),
        surface_format_(surface_format),
        swap_chain_extent_(swap_chain_extent),
        swap_chain_(std::move(swap_chain)),
        swap_chain_images_(swap_chain_images),
        swap_chain_image_views_(std::move(swap_chain_image_views)),
        render_pass_(std::move(render_pass)),
        depth_image_(std::move(depth_image)),
        depth_image_memory_(std::move(depth_image_memory)),
        depth_image_view_(std::move(depth_image_view)),
        swap_chain_framebuffers_(std::move(swap_chain_framebuffers)),
        command_pool_(std::move(command_pool)),
        command_buffers_(command_buffers),
        image_available_semaphore_(std::move(image_available_semaphore)),
        render_finished_semaphore_(std::move(render_finished_semaphore)) {
    frame_resources_.resize(command_buffers_.size());
  }

  VkInstance instance_;
  VkSurfaceKHR surface_;
  VkPhysicalDevice physical_device_;
  uint32_t graphics_queue_family_index_;
  uint32_t present_queue_family_index_;
  WithDeleter<VkDevice> device_;
  VkSurfaceFormatKHR surface_format_;
  VkExtent2D swap_chain_extent_;
  WithDeleter<VkSwapchainKHR> swap_chain_;
  std::vector<VkImage> swap_chain_images_;
  std::vector<WithDeleter<VkImageView>> swap_chain_image_views_;
  WithDeleter<VkRenderPass> render_pass_;
  WithDeleter<VkImage> depth_image_;
  WithDeleter<VkDeviceMemory> depth_image_memory_;
  WithDeleter<VkImageView> depth_image_view_;
  std::vector<WithDeleter<VkFramebuffer>> swap_chain_framebuffers_;
  WithDeleter<VkCommandPool> command_pool_;
  std::vector<VkCommandBuffer> command_buffers_;
  WithDeleter<VkSemaphore> image_available_semaphore_;
  WithDeleter<VkSemaphore> render_finished_semaphore_;

  std::vector<std::optional<FrameResources>> frame_resources_;

  bool has_rendered_ = false;
};

}  // namespace vulkan_utils

#endif  // _REIFY_SRC_VULKAN_UTILS_INCLUDE_VULKAN_UTILS_SWAP_CHAIN_RENDERER_H_
