#ifndef _IDE_VULKAN_LINE_RENDERER2_H
#define _IDE_VULKAN_LINE_RENDERER2_H

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

class LineRenderer2 {
 public:
  struct LineSegmentSoup {
    using Vector2 = std::array<float, 2>;
    struct Vertex {
      Vector2 position;
    };
    using Index = uint32_t;
    using LineSegment = std::array<Index, 2>;

    std::vector<Vertex> vertices;
    std::vector<LineSegment> line_segments;
  };

  static vulkan_utils::ErrorOr<LineRenderer2> Create(
      VkInstance instance, VkPhysicalDevice physical_device, VkDevice device,
      VkFormat output_image_format, VkRenderPass render_pass);

  LineRenderer2(LineRenderer2&& other) = default;
  ~LineRenderer2();

  std::optional<vulkan_utils::Error> SetLineSegmentSoup(
      std::shared_ptr<const LineSegmentSoup> line_segment_soup);

  vulkan_utils::ErrorOr<vulkan_utils::FrameResources> RenderFrame(
      VkCommandBuffer command_buffer, const glm::mat3& projection_view_matrix,
      const glm::vec4& color);

 private:
  struct LineRenderer2ConstructorData {
    VkInstance instance;
    VkPhysicalDevice physical_device;
    VkDevice device;

    WithDeleter<VkDescriptorSetLayout> descriptor_set_layout;

    WithDeleter<VkPipelineCache> pipeline_cache;
    WithDeleter<VkPipelineLayout> pipeline_layout;

    std::shared_ptr<WithDeleter<VkPipeline>> pipeline;
  };

  struct VulkanLineSegmentSoup {
    WithDeleter<VkBuffer> vertex_buffer;
    WithDeleter<VkDeviceMemory> vertex_buffer_memory;

    WithDeleter<VkBuffer> index_buffer;
    WithDeleter<VkDeviceMemory> index_buffer_memory;
  };

  LineRenderer2(LineRenderer2ConstructorData&& data) : data_(std::move(data)) {}

  LineRenderer2ConstructorData data_;

  std::shared_ptr<const LineSegmentSoup> line_segment_soup_;

  std::shared_ptr<VulkanLineSegmentSoup> vulkan_line_segment_soup_;
};

#endif  // _IDE_VULKAN_LINE_RENDERER2_H
