#ifndef _IDE_VULKAN_FLAT_TRIANGLE_RENDERER2_H
#define _IDE_VULKAN_FLAT_TRIANGLE_RENDERER2_H

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

class FlatTriangleRenderer2 {
 public:
  struct TriangleSoup {
    using Vector2 = std::array<float, 2>;
    struct Vertex {
      Vector2 position;
    };
    using Index = uint32_t;
    using Triangle = std::array<Index, 3>;

    std::vector<Vertex> vertices;
    std::vector<Triangle> triangles;
  };

  static vulkan_utils::ErrorOr<FlatTriangleRenderer2> Create(
      VkInstance instance, VkPhysicalDevice physical_device, VkDevice device,
      VkFormat output_image_format, VkRenderPass render_pass);

  FlatTriangleRenderer2(FlatTriangleRenderer2&& other) = default;
  ~FlatTriangleRenderer2();

  std::optional<vulkan_utils::Error> SetTriangleSoup(
      std::shared_ptr<const TriangleSoup> triangle_soup);

  vulkan_utils::ErrorOr<vulkan_utils::FrameResources> RenderFrame(
      VkCommandBuffer command_buffer, const glm::mat3& projection_view_matrix);

 private:
  struct FlatTriangleRenderer2ConstructorData {
    VkInstance instance;
    VkPhysicalDevice physical_device;
    VkDevice device;

    WithDeleter<VkDescriptorSetLayout> descriptor_set_layout;

    WithDeleter<VkPipelineCache> pipeline_cache;
    WithDeleter<VkPipelineLayout> pipeline_layout;

    std::shared_ptr<WithDeleter<VkPipeline>> pipeline;
  };

  struct VulkanTriangleSoup {
    WithDeleter<VkBuffer> vertex_buffer;
    WithDeleter<VkDeviceMemory> vertex_buffer_memory;

    WithDeleter<VkBuffer> index_buffer;
    WithDeleter<VkDeviceMemory> index_buffer_memory;
  };

  FlatTriangleRenderer2(FlatTriangleRenderer2ConstructorData&& data)
      : data_(std::move(data)) {}

  FlatTriangleRenderer2ConstructorData data_;

  std::shared_ptr<const TriangleSoup> triangle_soup_;

  std::shared_ptr<VulkanTriangleSoup> vulkan_triangle_soup_;
};

#endif  // _IDE_VULKAN_FLAT_TRIANGLE_RENDERER2_H
