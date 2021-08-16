#ifndef _IDE_VULKAN_FLAT_SHADED_TRIANGLE_RENDERER3_H
#define _IDE_VULKAN_FLAT_SHADED_TRIANGLE_RENDERER3_H

#include <vulkan/vulkan.h>

#include <any>
#include <array>
#include <functional>
#include <glm/glm.hpp>
#include <memory>
#include <optional>
#include <utility>
#include <variant>

#include "vulkan_utils/vulkan_utils.h"

using vulkan_utils::WithDeleter;

class FlatShadedTriangleRenderer3 {
 public:
  struct TriangleSoup {
    using Vector3 = std::array<float, 3>;
    struct Vertex {
      Vector3 position;
      Vector3 normal;
    };
    using Index = uint32_t;
    using Triangle = std::array<Index, 3>;

    std::vector<Vertex> vertices;
    std::vector<Triangle> triangles;
  };

  static vulkan_utils::ErrorOr<FlatShadedTriangleRenderer3> Create(
      VkInstance instance, VkPhysicalDevice physical_device, VkDevice device,
      VkFormat output_image_format, VkRenderPass render_pass);

  FlatShadedTriangleRenderer3(FlatShadedTriangleRenderer3&& other) = default;
  ~FlatShadedTriangleRenderer3();

  std::optional<vulkan_utils::Error> SetTriangleSoup(
      std::shared_ptr<const TriangleSoup> triangle_soup);

  vulkan_utils::ErrorOr<vulkan_utils::FrameResources> RenderFrame(
      VkCommandBuffer command_buffer, const glm::mat4& projection_view_matrix);

 private:
  struct FlatShadedTriangleRenderer3ConstructorData {
    VkInstance instance;
    VkPhysicalDevice physical_device;
    VkDevice device;

    WithDeleter<VkDescriptorSetLayout> descriptor_set_layout;

    WithDeleter<VkPipelineCache> pipeline_cache;
    WithDeleter<VkPipelineLayout> pipeline_layout;

    std::shared_ptr<WithDeleter<VkPipeline>> pipeline;
  };

  FlatShadedTriangleRenderer3(FlatShadedTriangleRenderer3ConstructorData&& data)
      : data_(std::move(data)) {}

  FlatShadedTriangleRenderer3ConstructorData data_;

  std::shared_ptr<const TriangleSoup> triangle_soup_;

  struct VulkanTriangleSoup {
    WithDeleter<VkBuffer> vertex_buffer;
    WithDeleter<VkDeviceMemory> vertex_buffer_memory;

    WithDeleter<VkBuffer> index_buffer;
    WithDeleter<VkDeviceMemory> index_buffer_memory;
  };
  std::shared_ptr<VulkanTriangleSoup> vulkan_triangle_soup_;
};

#endif  // _IDE_VULKAN_FLAT_SHADED_TRIANGLE_RENDERER3_H
