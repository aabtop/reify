#include "polygon_region_renderer.h"

#include <cstring>
#include <glm/glm.hpp>
#include <glm/gtc/matrix_transform.hpp>
#include <iostream>

namespace {

#include "src_gen/color_frag.h"
#include "src_gen/region2_solid_fill_vert.h"

struct MvpUniform {
  alignas(16) glm::mat4 model;
  alignas(16) glm::mat4 view_matrix;
};

}  // namespace

// static
vulkan_utils::ErrorOr<PolygonRegionRenderer> PolygonRegionRenderer::Create(
    VkInstance instance, VkPhysicalDevice physical_device, VkDevice device,
    VkFormat output_image_format, VkRenderPass render_pass) {
  VULKAN_UTILS_ASSIGN_OR_RETURN(
      descriptor_set_layout,
      vulkan_utils::MakeDescriptorSetLayout(
          device,
          {VkDescriptorSetLayoutBinding{0, VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER, 1,
                                        VK_SHADER_STAGE_VERTEX_BIT, nullptr}}));

  // Pipeline cache
  VULKAN_UTILS_ASSIGN_OR_RETURN(pipeline_cache,
                                vulkan_utils::MakePipelineCache(device));

  // Pipeline layout
  VULKAN_UTILS_ASSIGN_OR_RETURN(
      pipeline_layout,
      vulkan_utils::MakePipelineLayout(device, descriptor_set_layout.value()));

  // Shaders
  VULKAN_UTILS_ASSIGN_OR_RETURN(
      vertex_shader_module,
      vulkan_utils::CreateShader(device, region2_solid_fill_vert_spv,
                                 region2_solid_fill_vert_spv_len));
  VULKAN_UTILS_ASSIGN_OR_RETURN(
      fragment_shader_module,
      vulkan_utils::CreateShader(device, color_frag_spv, color_frag_spv_len));

  // Graphics pipeline
  VULKAN_UTILS_ASSIGN_OR_RETURN(
      pipeline, vulkan_utils::MakePipeline(device, pipeline_layout.value(),
                                           render_pass, pipeline_cache.value(),
                                           vertex_shader_module.value(),
                                           {
                                               {
                                                   0,  // binding
                                                   2 * sizeof(float),
                                                   VK_VERTEX_INPUT_RATE_VERTEX,
                                               },
                                           },
                                           {
                                               {
                                                   // position
                                                   0,  // location
                                                   0,  // binding
                                                   VK_FORMAT_R32G32_SFLOAT,
                                                   0,
                                               },
                                           },
                                           fragment_shader_module.value()));

  return PolygonRegionRenderer(PolygonRegionRendererConstructorData{
      instance,
      physical_device,
      device,
      std::move(descriptor_set_layout),
      std::move(pipeline_cache),
      std::move(pipeline_layout),
      std::make_shared<WithDeleter<VkPipeline>>(std::move(pipeline)),
  });
}

PolygonRegionRenderer::~PolygonRegionRenderer() {}

auto PolygonRegionRenderer::RenderFrame(VkCommandBuffer command_buffer,
                                        const glm::mat4& projection_view_matrix)
    -> vulkan_utils::ErrorOr<vulkan_utils::FrameResources> {
  glm::mat4 model_matrix = glm::mat4(1.0f);
  MvpUniform uniform_data{model_matrix, projection_view_matrix};

  VULKAN_UTILS_ASSIGN_OR_RETURN(
      uniform_buffer,
      vulkan_utils::MakeBuffer(data_.device, VK_BUFFER_USAGE_UNIFORM_BUFFER_BIT,
                               sizeof(uniform_data)));
  VULKAN_UTILS_ASSIGN_OR_RETURN(
      uniform_buffer_memory,
      vulkan_utils::AllocateAndBindBufferMemory(
          data_.physical_device, data_.device, uniform_buffer.value(),
          reinterpret_cast<uint8_t*>(&uniform_data), sizeof(uniform_data)));

  // Set up descriptor set and its layout.
  VULKAN_UTILS_ASSIGN_OR_RETURN(
      descriptor_pool,
      vulkan_utils::MakeDescriptorPool(
          data_.device,
          {VkDescriptorPoolSize{VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER, 1}}, 1));

  VULKAN_UTILS_ASSIGN_OR_RETURN(
      uniform_descriptor_set,
      vulkan_utils::MakeDescriptorSet(
          data_.device, descriptor_pool.value(),
          data_.descriptor_set_layout.value(),
          {VkDescriptorBufferInfo{uniform_buffer.value(), 0,
                                  sizeof(MvpUniform)}}));

  if (vulkan_triangle_soup_) {
    vkCmdBindPipeline(command_buffer, VK_PIPELINE_BIND_POINT_GRAPHICS,
                      data_.pipeline->value());
    vkCmdBindDescriptorSets(command_buffer, VK_PIPELINE_BIND_POINT_GRAPHICS,
                            data_.pipeline_layout.value(), 0, 1,
                            &(uniform_descriptor_set.value()), 0, nullptr);

    VkDeviceSize vertex_buffer_offset = 0;
    vkCmdBindVertexBuffers(command_buffer, 0, 1,
                           &(vulkan_triangle_soup_->vertex_buffer.value()),
                           &vertex_buffer_offset);
    vkCmdBindIndexBuffer(command_buffer,
                         vulkan_triangle_soup_->index_buffer.value(), 0,
                         VK_INDEX_TYPE_UINT32);

    vkCmdDrawIndexed(
        command_buffer,
        static_cast<uint32_t>(triangle_soup_->triangles.size() * 3), 1, 0, 0,
        0);
  }

  auto resources = std::make_tuple(
      vulkan_triangle_soup_, std::move(uniform_descriptor_set),
      std::move(descriptor_pool), std::move(uniform_buffer_memory),
      std::move(uniform_buffer), data_.pipeline);
  // std::any doesn't support move only types, so we wrap it in a shared_ptr.
  return vulkan_utils::FrameResources(
      std::make_shared<decltype(resources)>(std::move(resources)));
}

std::optional<vulkan_utils::Error> PolygonRegionRenderer::SetTriangleSoup(
    std::shared_ptr<const TriangleSoup> triangle_soup) {
  triangle_soup_ = triangle_soup;
  if (!triangle_soup_) {
    vulkan_triangle_soup_ = nullptr;
    return std::nullopt;
  }

  size_t vertex_buffer_size =
      triangle_soup_->vertices.size() * sizeof(triangle_soup_->vertices[0]);

  VULKAN_UTILS_ASSIGN_OR_RETURN(
      vertex_buffer,
      vulkan_utils::MakeBuffer(data_.device, VK_BUFFER_USAGE_VERTEX_BUFFER_BIT,
                               vertex_buffer_size));
  VULKAN_UTILS_ASSIGN_OR_RETURN(
      vertex_buffer_memory,
      vulkan_utils::AllocateAndBindBufferMemory(
          data_.physical_device, data_.device, vertex_buffer.value(),
          reinterpret_cast<const uint8_t*>(triangle_soup_->vertices.data()),
          vertex_buffer_size));

  size_t index_buffer_size =
      triangle_soup_->triangles.size() * sizeof(triangle_soup_->triangles[0]);
  VULKAN_UTILS_ASSIGN_OR_RETURN(
      index_buffer,
      vulkan_utils::MakeBuffer(data_.device, VK_BUFFER_USAGE_INDEX_BUFFER_BIT,
                               index_buffer_size));
  VULKAN_UTILS_ASSIGN_OR_RETURN(
      index_buffer_memory,
      vulkan_utils::AllocateAndBindBufferMemory(
          data_.physical_device, data_.device, index_buffer.value(),
          reinterpret_cast<const uint8_t*>(triangle_soup_->triangles.data()),
          index_buffer_size));

  vulkan_triangle_soup_ =
      std::shared_ptr<VulkanTriangleSoup>(new VulkanTriangleSoup{
          std::move(vertex_buffer),
          std::move(vertex_buffer_memory),
          std::move(index_buffer),
          std::move(index_buffer_memory),
      });

  return std::nullopt;
}
