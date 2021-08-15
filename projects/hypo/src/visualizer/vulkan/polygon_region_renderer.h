#ifndef _IDE_VULKAN_POLYGON_REGION_RENDERER_H
#define _IDE_VULKAN_POLYGON_REGION_RENDERER_H

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

class PolygonRegionRenderer {
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

  static vulkan_utils::ErrorOr<PolygonRegionRenderer> Create(
      VkInstance instance, VkPhysicalDevice physical_device, VkDevice device,
      VkFormat output_image_format, VkRenderPass render_pass);

  PolygonRegionRenderer(PolygonRegionRenderer&& other) = default;
  ~PolygonRegionRenderer();

  std::optional<vulkan_utils::Error> SetTriangleSoup(
      std::shared_ptr<const TriangleSoup> triangle_soup);

  vulkan_utils::ErrorOr<vulkan_utils::FrameResources> RenderFrame(
      VkCommandBuffer command_buffer, const glm::mat4& projection_view_matrix);

 private:
  struct PolygonRegionRendererConstructorData {
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

  PolygonRegionRenderer(PolygonRegionRendererConstructorData&& data)
      : data_(std::move(data)) {}

  PolygonRegionRendererConstructorData data_;

  std::shared_ptr<const TriangleSoup> triangle_soup_;

  std::shared_ptr<VulkanTriangleSoup> vulkan_triangle_soup_;
};

#endif  // _IDE_VULKAN_POLYGON_REGION_RENDERER_H
