#include "simple_simplex_renderer2.h"

#include <cstring>
#include <glm/glm.hpp>
#include <glm/gtc/matrix_transform.hpp>
#include <iostream>

namespace {

#include "src_gen/color_frag.h"
#include "src_gen/region2_solid_fill_vert.h"

struct MvpUniform {
  // We set the type as a mat4 to make alignment requirements happy, but we're
  // actually just stuffing mat3s in here.
  alignas(16) glm::mat4 model;
  alignas(16) glm::mat4 view_matrix;
};

}  // namespace

// static
vulkan_utils::ErrorOr<SimpleSimplexRenderer2> SimpleSimplexRenderer2::Create(
    VkInstance instance, VkPhysicalDevice physical_device, VkDevice device,
    VkFormat output_image_format, VkRenderPass render_pass,
    int embedded_dimension) {
  VULKAN_UTILS_ASSIGN_OR_RETURN(
      descriptor_set_layout,
      vulkan_utils::MakeDescriptorSetLayout(
          device,
          {VkDescriptorSetLayoutBinding{0, VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER, 1,
                                        VK_SHADER_STAGE_VERTEX_BIT, nullptr},
           VkDescriptorSetLayoutBinding{1, VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER, 1,
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
  vulkan_utils::MakePipelineOptions make_pipeline_options;
  make_pipeline_options.depth_test = false;
  make_pipeline_options.depth_write = false;
  if (embedded_dimension == 1) {
    make_pipeline_options.primitive_topology = VK_PRIMITIVE_TOPOLOGY_LINE_LIST;
  } else if (embedded_dimension == 2) {
    make_pipeline_options.primitive_topology =
        VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST;
  } else {
    assert(false);
  }
  VULKAN_UTILS_ASSIGN_OR_RETURN(
      pipeline, vulkan_utils::MakePipeline(
                    device, pipeline_layout.value(), render_pass,
                    pipeline_cache.value(), vertex_shader_module.value(),
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
                    fragment_shader_module.value(), make_pipeline_options));

  return SimpleSimplexRenderer2(SimpleSimplexRenderer2ConstructorData{
      instance,
      physical_device,
      device,
      std::move(descriptor_set_layout),
      std::move(pipeline_cache),
      std::move(pipeline_layout),
      std::make_shared<WithDeleter<VkPipeline>>(std::move(pipeline)),
  });
}

SimpleSimplexRenderer2::~SimpleSimplexRenderer2() {}

auto SimpleSimplexRenderer2::RenderFrame(
    VkCommandBuffer command_buffer, const glm::mat3& projection_view_matrix,
    const glm::vec4& color)
    -> vulkan_utils::ErrorOr<vulkan_utils::FrameResources> {
  glm::mat4 model_matrix = glm::mat4(1.0f);
  MvpUniform mvp_uniform_data{model_matrix, glm::mat4(projection_view_matrix)};

  VULKAN_UTILS_ASSIGN_OR_RETURN(
      mvp_uniform_buffer,
      vulkan_utils::MakeBuffer(data_.device, VK_BUFFER_USAGE_UNIFORM_BUFFER_BIT,
                               sizeof(mvp_uniform_data)));
  VULKAN_UTILS_ASSIGN_OR_RETURN(
      mvp_uniform_buffer_memory,
      vulkan_utils::AllocateAndBindBufferMemory(
          data_.physical_device, data_.device, mvp_uniform_buffer.value(),
          reinterpret_cast<uint8_t*>(&mvp_uniform_data),
          sizeof(mvp_uniform_data)));

  VULKAN_UTILS_ASSIGN_OR_RETURN(
      color_uniform_buffer,
      vulkan_utils::MakeBuffer(data_.device, VK_BUFFER_USAGE_UNIFORM_BUFFER_BIT,
                               sizeof(color)));
  VULKAN_UTILS_ASSIGN_OR_RETURN(
      color_uniform_buffer_memory,
      vulkan_utils::AllocateAndBindBufferMemory(
          data_.physical_device, data_.device, color_uniform_buffer.value(),
          reinterpret_cast<const uint8_t*>(&color), sizeof(color)));

  // Set up descriptor set and its layout.
  VULKAN_UTILS_ASSIGN_OR_RETURN(
      descriptor_pool,
      vulkan_utils::MakeDescriptorPool(
          data_.device,
          {VkDescriptorPoolSize{VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER, 1},
           VkDescriptorPoolSize{VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER, 1}},
          1));

  VULKAN_UTILS_ASSIGN_OR_RETURN(
      uniform_descriptor_set,
      vulkan_utils::MakeDescriptorSet(
          data_.device, descriptor_pool.value(),
          data_.descriptor_set_layout.value(),
          {VkDescriptorBufferInfo{mvp_uniform_buffer.value(), 0,
                                  sizeof(MvpUniform)},
           VkDescriptorBufferInfo{color_uniform_buffer.value(), 0,
                                  sizeof(glm::vec4)}}));

  if (simplex_soup_vulkan_buffers_) {
    vkCmdBindPipeline(command_buffer, VK_PIPELINE_BIND_POINT_GRAPHICS,
                      data_.pipeline->value());
    vkCmdBindDescriptorSets(command_buffer, VK_PIPELINE_BIND_POINT_GRAPHICS,
                            data_.pipeline_layout.value(), 0, 1,
                            &(uniform_descriptor_set.value()), 0, nullptr);

    VkDeviceSize vertex_buffer_offset = 0;
    vkCmdBindVertexBuffers(
        command_buffer, 0, 1,
        &(simplex_soup_vulkan_buffers_->vertex_buffer.value()),
        &vertex_buffer_offset);
    vkCmdBindIndexBuffer(command_buffer,
                         simplex_soup_vulkan_buffers_->index_buffer.value(), 0,
                         VK_INDEX_TYPE_UINT32);

    vkCmdDrawIndexed(
        command_buffer,
        static_cast<uint32_t>(simplex_soup_vulkan_buffers_->indices_count), 1,
        0, 0, 0);
  }

  auto resources = std::make_tuple(
      simplex_soup_vulkan_buffers_, std::move(uniform_descriptor_set),
      std::move(descriptor_pool), std::move(mvp_uniform_buffer_memory),
      std::move(mvp_uniform_buffer), std::move(color_uniform_buffer_memory),
      std::move(color_uniform_buffer), data_.pipeline);
  // std::any doesn't support move only types, so we wrap it in a shared_ptr.
  return vulkan_utils::FrameResources(
      std::make_shared<decltype(resources)>(std::move(resources)));
}

std::optional<vulkan_utils::Error>
SimpleSimplexRenderer2::SetSimplexSoupInternal(
    int embedder_dimension, int embedded_dimension, const void* vertex_data,
    size_t vertex_data_size, const void* simplex_data, size_t simplex_data_size,
    size_t indices_count) {
  VULKAN_UTILS_ASSIGN_OR_RETURN(
      vertex_buffer,
      vulkan_utils::MakeBuffer(data_.device, VK_BUFFER_USAGE_VERTEX_BUFFER_BIT,
                               vertex_data_size));
  VULKAN_UTILS_ASSIGN_OR_RETURN(
      vertex_buffer_memory,
      vulkan_utils::AllocateAndBindBufferMemory(
          data_.physical_device, data_.device, vertex_buffer.value(),
          reinterpret_cast<const uint8_t*>(vertex_data), vertex_data_size));

  VULKAN_UTILS_ASSIGN_OR_RETURN(
      index_buffer,
      vulkan_utils::MakeBuffer(data_.device, VK_BUFFER_USAGE_INDEX_BUFFER_BIT,
                               simplex_data_size));
  VULKAN_UTILS_ASSIGN_OR_RETURN(
      index_buffer_memory,
      vulkan_utils::AllocateAndBindBufferMemory(
          data_.physical_device, data_.device, index_buffer.value(),
          reinterpret_cast<const uint8_t*>(simplex_data), simplex_data_size));

  simplex_soup_vulkan_buffers_ =
      std::shared_ptr<SimplexSoupVulkanBuffers>(new SimplexSoupVulkanBuffers{
          std::move(vertex_buffer), std::move(vertex_buffer_memory),
          std::move(index_buffer), std::move(index_buffer_memory),
          indices_count});

  // Success!
  return std::nullopt;
}

void SimpleSimplexRenderer2::ClearSimplexSoup() {
  simplex_soup_vulkan_buffers_ = nullptr;
}