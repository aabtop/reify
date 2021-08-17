#ifndef _REIFY_SRC_VULKAN_UTILS_INCLUDE_VULKAN_UTILS_VULKAN_UTILS_H_
#define _REIFY_SRC_VULKAN_UTILS_INCLUDE_VULKAN_UTILS_VULKAN_UTILS_H_

#include <vulkan/vulkan.h>

#include <any>
#include <functional>
#include <optional>
#include <string>
#include <variant>

#define VULKAN_UTILS_ASSIGN_OR_RETURN(lhs, rhs)    \
  auto maybe_##lhs = rhs;                          \
  if (auto error = std::get_if<0>(&maybe_##lhs)) { \
    return *error;                                 \
  }                                                \
  auto& lhs = std::get<1>(maybe_##lhs)

namespace vulkan_utils {

template <typename T>
class WithDeleter {
 public:
  WithDeleter(T&& value, const std::function<void(T&&)>& deleter)
      : value_(std::move(value)), deleter_(deleter) {}
  WithDeleter(WithDeleter&&) = default;
  WithDeleter(const WithDeleter&) = delete;
  WithDeleter& operator=(const WithDeleter&) = delete;
  WithDeleter& operator=(WithDeleter&&) = default;

  // Move an existing value but use a different deleter.
  WithDeleter(WithDeleter&& other, const std::function<void(T&&)>& deleter)
      : value_(std::move(other.value_)), deleter_(deleter) {}

  ~WithDeleter() {
    if (deleter_) {
      deleter_(std::move(value_));
    }
  }

  const T& value() const { return value_; }

 private:
  T value_;
  std::function<void(T&&)> deleter_;
};

struct Error {
  std::string msg;
  VkResult vk_result = VK_ERROR_UNKNOWN;
};
template <typename T>
using ErrorOr = std::variant<Error, T>;
using FrameResources = std::any;

ErrorOr<uint32_t> FindMemoryTypeIndex(VkPhysicalDevice physical_device,
                                      uint32_t type_filter,
                                      VkMemoryPropertyFlags properties);

ErrorOr<WithDeleter<VkShaderModule>> CreateShader(VkDevice device,
                                                  const uint8_t* data,
                                                  size_t size);

ErrorOr<WithDeleter<VkDescriptorPool>> MakeDescriptorPool(
    VkDevice device,
    const std::vector<VkDescriptorPoolSize> descriptor_pool_sizes,
    uint32_t max_sets);

ErrorOr<WithDeleter<VkDescriptorSetLayout>> MakeDescriptorSetLayout(
    VkDevice device, std::vector<VkDescriptorSetLayoutBinding> layout_bindings);

ErrorOr<WithDeleter<VkDescriptorSet>> MakeDescriptorSet(
    VkDevice device, VkDescriptorPool pool, VkDescriptorSetLayout layout,
    const std::vector<
        std::variant<VkDescriptorBufferInfo, VkDescriptorImageInfo>>&
        descriptor_set_write_infos);

ErrorOr<WithDeleter<VkDeviceMemory>> Allocate(VkDevice device,
                                              VkDeviceSize size,
                                              uint32_t memory_type);

ErrorOr<WithDeleter<VkDeviceMemory>> Allocate(
    VkPhysicalDevice physical_device, VkDevice device, VkDeviceSize size,
    uint32_t type_filter, VkMemoryPropertyFlags memory_property_flags);

ErrorOr<WithDeleter<VkBuffer>> MakeBuffer(VkDevice device,
                                          VkBufferUsageFlagBits usage_flags,
                                          size_t data_size);

ErrorOr<WithDeleter<VkDeviceMemory>> AllocateAndBindBufferMemory(
    VkPhysicalDevice physical_device, VkDevice device, VkBuffer buffer,
    const uint8_t* data, size_t data_size);

ErrorOr<WithDeleter<VkImage>> MakeImage(VkDevice device, uint32_t width,
                                        uint32_t height, VkFormat format,
                                        VkImageTiling tiling,
                                        VkImageUsageFlags usage);

ErrorOr<WithDeleter<VkDeviceMemory>> AllocateAndBindImageMemory(
    VkPhysicalDevice physical_device, VkDevice device, VkImage image,
    VkMemoryPropertyFlags properties);

void SetImageLayout(VkCommandBuffer command_buffer, VkImage image,
                    VkImageAspectFlags image_aspect_flags,
                    VkImageLayout old_layout, VkImageLayout new_layout);

ErrorOr<WithDeleter<VkImageView>> MakeImageView(VkDevice device, VkImage image,
                                                VkFormat format,
                                                VkImageAspectFlags aspect_mask);

ErrorOr<WithDeleter<VkPipelineCache>> MakePipelineCache(VkDevice device);

ErrorOr<WithDeleter<VkPipelineLayout>> MakePipelineLayout(
    VkDevice device, VkDescriptorSetLayout descriptor_set_layout);

ErrorOr<WithDeleter<VkRenderPass>> MakeRenderPass(
    VkDevice device,
    const std::vector<VkAttachmentDescription>& color_attachments,
    const std::optional<VkAttachmentDescription>&
        maybe_depth_stencil_attachment);

struct MakePipelineOptions {
  VkPrimitiveTopology primitive_topology = VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST;
  float line_width = 1.0f;
  bool depth_write = true;
  bool depth_test = true;
};

ErrorOr<WithDeleter<VkPipeline>> MakePipeline(
    VkDevice device, VkPipelineLayout pipeline_layout, VkRenderPass render_pass,
    VkPipelineCache pipeline_cache, VkShaderModule vertex_shader_module,
    const std::vector<VkVertexInputBindingDescription>&
        vertex_input_binding_descriptions,
    const std::vector<VkVertexInputAttributeDescription>&
        vertex_input_attribute_description,
    VkShaderModule fragment_shader_module,
    const MakePipelineOptions& options = MakePipelineOptions());

ErrorOr<WithDeleter<VkSemaphore>> MakeSemaphore(VkDevice device);

ErrorOr<WithDeleter<VkDebugUtilsMessengerEXT>> MakeDebugUtilsMessenger(
    VkInstance instance, VkDebugUtilsMessageSeverityFlagBitsEXT filter_severity,
    VkDebugUtilsMessageTypeFlagsEXT filter_message_type,
    PFN_vkDebugUtilsMessengerCallbackEXT user_callback);

ErrorOr<WithDeleter<VkInstance>> MakeInstance(
    const VkApplicationInfo& application_info,
    const std::vector<const char*>& instance_extensions);

ErrorOr<std::vector<VkPhysicalDevice>> EnumeratePhysicalDevices(
    VkInstance instance);

std::optional<uint32_t> FindFirstQueueFamilyIndex(
    VkPhysicalDevice physical_device, VkQueueFlagBits queue_family_bits);

ErrorOr<WithDeleter<VkDevice>> MakeDevice(
    VkPhysicalDevice physical_device, uint32_t device_queue,
    const std::vector<const char*>& device_extensions);

ErrorOr<WithDeleter<VkSwapchainKHR>> MakeSwapChain(
    VkDevice device, VkSurfaceKHR surface, uint32_t min_image_count,
    VkSurfaceFormatKHR surface_format, VkPresentModeKHR present_mode,
    VkExtent2D swap_chain_extent, VkSurfaceTransformFlagBitsKHR pre_transform,
    uint32_t graphics_queue_family_index, uint32_t present_queue_family_index);

ErrorOr<WithDeleter<VkFramebuffer>> MakeFramebuffer(
    VkDevice device, VkRenderPass render_pass,
    const std::vector<VkImageView>& color_attachments,
    const std::optional<VkImageView>& maybe_depth_stencil_attachment,
    VkExtent2D extent);

ErrorOr<WithDeleter<VkCommandPool>> MakeCommandPool(
    VkDevice device, uint32_t queue_family_index);

ErrorOr<std::vector<VkCommandBuffer>> MakeCommandBuffers(
    VkDevice device, VkCommandPool command_pool, uint32_t num_command_buffers);

}  // namespace vulkan_utils

#endif  // _REIFY_SRC_VULKAN_UTILS_INCLUDE_VULKAN_UTILS_VULKAN_UTILS_H_
