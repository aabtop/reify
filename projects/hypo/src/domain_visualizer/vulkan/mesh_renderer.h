#ifndef _IDE_VULKAN_MESH_RENDERER_H
#define _IDE_VULKAN_MESH_RENDERER_H

#include <vulkan/vulkan.h>

#include <any>
#include <array>
#include <functional>
#include <glm/glm.hpp>
#include <memory>
#include <optional>
#include <utility>
#include <variant>

#include "triangle_soup.h"
#include "vulkan_utils/vulkan_utils.h"

using vulkan_utils::WithDeleter;

class MeshRenderer {
 public:
  using Error = vulkan_utils::Error;
  template <typename T>
  using ErrorOr = vulkan_utils::ErrorOr<T>;
  using FrameResources = vulkan_utils::FrameResources;

  static ErrorOr<MeshRenderer> Create(VkInstance instance,
                                      VkPhysicalDevice physical_device,
                                      VkDevice device,
                                      VkFormat output_image_format);

  MeshRenderer(MeshRenderer&& other) = default;
  ~MeshRenderer();

  std::optional<Error> SetTriangleSoup(
      std::shared_ptr<const TriangleSoup> triangle_soup);

  ErrorOr<FrameResources> RenderFrame(
      VkCommandBuffer command_buffer, VkFramebuffer framebuffer,
      const std::array<uint32_t, 2>& output_surface_size,
      const glm::mat4& view_matrix);

 private:
  struct MeshRendererConstructorData {
    VkInstance instance;
    VkPhysicalDevice physical_device;
    VkDevice device;

    WithDeleter<VkDescriptorSetLayout> descriptor_set_layout;

    WithDeleter<VkPipelineCache> pipeline_cache;
    WithDeleter<VkPipelineLayout> pipeline_layout;

    WithDeleter<VkRenderPass> render_pass;
    WithDeleter<VkPipeline> pipeline;
  };

  MeshRenderer(MeshRendererConstructorData&& data) : data_(std::move(data)) {}

  MeshRendererConstructorData data_;

  std::shared_ptr<const TriangleSoup> triangle_soup_;

  struct VulkanTriangleSoup {
    WithDeleter<VkBuffer> vertex_buffer;
    WithDeleter<VkDeviceMemory> vertex_buffer_memory;

    WithDeleter<VkBuffer> index_buffer;
    WithDeleter<VkDeviceMemory> index_buffer_memory;
  };
  std::shared_ptr<VulkanTriangleSoup> vulkan_triangle_soup_;
};

#endif  // _IDE_VULKAN_MESH_RENDERER_H
