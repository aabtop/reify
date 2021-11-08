#ifndef _IDE_VULKAN_SIMPLE_SIMPLEX_RENDERER2_H
#define _IDE_VULKAN_SIMPLE_SIMPLEX_RENDERER2_H

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

class SimpleSimplexRenderer2 {
 public:
  template <int EmbedderDimension, int EmbeddedDimension>
  struct SimplexSoup {
    using Point = std::array<float, EmbedderDimension>;
    struct Vertex {
      Point position;
    };
    using Index = uint32_t;
    using Simplex =
        std::array<Index, EmbeddedDimension +
                              1>;  // E.g. "Triangle" for EmbeddedDimension = 2

    std::vector<Vertex> vertices;
    std::vector<Simplex>
        simplices;  // E.g. "triangles" for EmbeddedDimension = 2
  };

  template <int EmbedderDimension, int EmbeddedDimension>
  static vulkan_utils::ErrorOr<SimpleSimplexRenderer2> Create(
      VkInstance instance, VkPhysicalDevice physical_device, VkDevice device,
      VkFormat output_image_format, VkRenderPass render_pass,
      const SimplexSoup<EmbedderDimension, EmbeddedDimension>& simplex_soup,
      const glm::vec4& color);

  SimpleSimplexRenderer2(const SimpleSimplexRenderer2& other) = delete;
  SimpleSimplexRenderer2(SimpleSimplexRenderer2&& other) = default;
  ~SimpleSimplexRenderer2();

  vulkan_utils::ErrorOr<vulkan_utils::FrameResources> RenderFrame(
      VkCommandBuffer command_buffer, const glm::mat3& projection_view_matrix);

 private:
  struct SimplexSoupVulkanBuffers {
    WithDeleter<VkBuffer> vertex_buffer;
    WithDeleter<VkDeviceMemory> vertex_buffer_memory;

    WithDeleter<VkBuffer> index_buffer;
    WithDeleter<VkDeviceMemory> index_buffer_memory;

    size_t indices_count;
  };

  struct SimpleSimplexRenderer2ConstructorData {
    VkInstance instance;
    VkPhysicalDevice physical_device;
    VkDevice device;

    WithDeleter<VkDescriptorSetLayout> descriptor_set_layout;

    WithDeleter<VkPipelineCache> pipeline_cache;
    WithDeleter<VkPipelineLayout> pipeline_layout;

    std::shared_ptr<WithDeleter<VkPipeline>> pipeline;

    glm::vec4 color;

    std::shared_ptr<SimplexSoupVulkanBuffers> simplex_soup_vulkan_buffer;
  };

  static vulkan_utils::ErrorOr<SimpleSimplexRenderer2> CreateInternal(
      VkInstance instance, VkPhysicalDevice physical_device, VkDevice device,
      VkFormat output_image_format, VkRenderPass render_pass,
      int embedded_dimension,
      std::shared_ptr<SimplexSoupVulkanBuffers> simplex_soup_vulkan_buffer,
      const glm::vec4& color);

  SimpleSimplexRenderer2(SimpleSimplexRenderer2ConstructorData&& data)
      : data_(std::move(data)) {}

  static vulkan_utils::ErrorOr<std::shared_ptr<SimplexSoupVulkanBuffers>>
  MakeSimplexSoupVulkanBuffers(int embedder_dimension, int embedded_dimension,
                               VkPhysicalDevice physical_device,
                               VkDevice device, const void* vertex_data,
                               size_t vertex_data_size,
                               const void* simplex_data,
                               size_t simplex_data_size, size_t indices_count);

  SimpleSimplexRenderer2ConstructorData data_;
};

template <int EmbedderDimension, int EmbeddedDimension>
vulkan_utils::ErrorOr<SimpleSimplexRenderer2> SimpleSimplexRenderer2::Create(
    VkInstance instance, VkPhysicalDevice physical_device, VkDevice device,
    VkFormat output_image_format, VkRenderPass render_pass,
    const SimplexSoup<EmbedderDimension, EmbeddedDimension>& simplex_soup,
    const glm::vec4& color) {
  // Move from template land to runtime land.
  VULKAN_UTILS_ASSIGN_OR_RETURN(
      simplex_soup_vulkan_buffer,
      MakeSimplexSoupVulkanBuffers(
          EmbedderDimension, EmbeddedDimension, physical_device, device,
          reinterpret_cast<const void*>(simplex_soup.vertices.data()),
          simplex_soup.vertices.size() * sizeof(simplex_soup.vertices[0]),
          reinterpret_cast<const void*>(simplex_soup.simplices.data()),
          simplex_soup.simplices.size() * sizeof(simplex_soup.simplices[0]),
          simplex_soup.simplices.size() * (EmbeddedDimension + 1)));

  return CreateInternal(instance, physical_device, device, output_image_format,
                        render_pass, EmbeddedDimension,
                        simplex_soup_vulkan_buffer, color);
}

#endif  // _IDE_VULKAN_SIMPLE_SIMPLEX_RENDERER2_H
