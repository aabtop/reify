#include "mesh_renderer.h"

#include <cstring>
#include <glm/glm.hpp>
#include <glm/gtc/matrix_transform.hpp>
#include <iostream>

namespace {

#include "src_gen/color_frag.h"
#include "src_gen/color_vert.h"

struct MvpUniform {
  alignas(16) glm::mat4 model;
  alignas(16) glm::mat4 view;
  alignas(16) glm::mat4 projection;
};

}  // namespace

// static
MeshRenderer::ErrorOr<MeshRenderer> MeshRenderer::Create(
    VkInstance instance, VkPhysicalDevice physical_device, VkDevice device,
    VkFormat output_image_format) {
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
      vulkan_utils::CreateShader(device, color_vert_spv, color_vert_spv_len));
  VULKAN_UTILS_ASSIGN_OR_RETURN(
      fragment_shader_module,
      vulkan_utils::CreateShader(device, color_frag_spv, color_frag_spv_len));

  // Render pass
  VULKAN_UTILS_ASSIGN_OR_RETURN(
      render_pass, vulkan_utils::MakeRenderPass(
                       device,
                       {VkAttachmentDescription{
                           0,
                           output_image_format,
                           VK_SAMPLE_COUNT_1_BIT,
                           VK_ATTACHMENT_LOAD_OP_CLEAR,
                           VK_ATTACHMENT_STORE_OP_STORE,
                           VK_ATTACHMENT_LOAD_OP_DONT_CARE,
                           VK_ATTACHMENT_STORE_OP_DONT_CARE,
                           VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL,
                           VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL,
                       }},
                       VkAttachmentDescription{
                           0,
                           VK_FORMAT_D32_SFLOAT,
                           VK_SAMPLE_COUNT_1_BIT,
                           VK_ATTACHMENT_LOAD_OP_CLEAR,
                           VK_ATTACHMENT_STORE_OP_DONT_CARE,
                           VK_ATTACHMENT_LOAD_OP_DONT_CARE,
                           VK_ATTACHMENT_STORE_OP_DONT_CARE,
                           VK_IMAGE_LAYOUT_UNDEFINED,
                           VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL,
                       }));

  // Graphics pipeline
  VULKAN_UTILS_ASSIGN_OR_RETURN(
      pipeline, vulkan_utils::MakePipeline(
                    device, pipeline_layout.value(), render_pass.value(),
                    pipeline_cache.value(), vertex_shader_module.value(),
                    {
                        {
                            0,  // binding
                            6 * sizeof(float),
                            VK_VERTEX_INPUT_RATE_VERTEX,
                        },
                    },
                    {
                        {
                            // position
                            0,  // location
                            0,  // binding
                            VK_FORMAT_R32G32B32_SFLOAT,
                            0,
                        },
                        {
                            // normal
                            1,  // location
                            0,  // binding
                            VK_FORMAT_R32G32B32_SFLOAT,
                            3 * sizeof(float),
                        },
                    },
                    fragment_shader_module.value()));

  return MeshRenderer(MeshRendererConstructorData{
      instance,
      physical_device,
      device,
      std::move(descriptor_set_layout),
      std::move(pipeline_cache),
      std::move(pipeline_layout),
      std::make_shared<WithDeleter<VkRenderPass>>(std::move(render_pass)),
      std::make_shared<WithDeleter<VkPipeline>>(std::move(pipeline)),
  });
}

MeshRenderer::~MeshRenderer() {}

auto MeshRenderer::RenderFrame(VkCommandBuffer command_buffer,
                               VkFramebuffer framebuffer,
                               VkImage output_color_image,
                               const std::array<int, 4>& viewport_region,
                               const glm::mat4& view_matrix)
    -> ErrorOr<FrameResources> {
  VkViewport viewport;
  viewport.x = viewport_region[0];
  viewport.y = viewport_region[1];
  viewport.width = viewport_region[2] - viewport_region[0];
  viewport.height = viewport_region[3] - viewport_region[1];
  viewport.minDepth = 0;
  viewport.maxDepth = 1;
  vkCmdSetViewport(command_buffer, 0, 1, &viewport);

  VkRect2D scissor;
  scissor.offset.x = viewport_region[0];
  scissor.offset.y = viewport_region[1];
  scissor.extent.width = viewport_region[2] - viewport_region[0];
  scissor.extent.height = viewport_region[3] - viewport_region[1];
  vkCmdSetScissor(command_buffer, 0, 1, &scissor);

  glm::mat4 projection_matrix =
      glm::perspective(
          45.0f,
          (viewport_region[2] - viewport_region[0]) /
              static_cast<float>(viewport_region[3] - viewport_region[1]),
          0.0001f, 10000.0f)
      // Flip the y and z axes so that positive y is up and positive z is away.
      * glm::scale(glm::mat4(1), glm::vec3(1.0f, -1.0f, -1.0f));
  glm::mat4 model_matrix = glm::mat4(1.0f);
  MvpUniform uniform_data{model_matrix, view_matrix, projection_matrix};

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

  std::array<VkClearValue, 2> clear_values{};
  memset(clear_values.data(), 0, sizeof(clear_values[0]) * clear_values.size());
  clear_values[0].color = {{0.11, 0.11, 0.11, 1}};
  clear_values[1].depthStencil = {1, 0};

  VkRenderPassBeginInfo rpb{};
  rpb.sType = VK_STRUCTURE_TYPE_RENDER_PASS_BEGIN_INFO;
  rpb.renderPass = data_.render_pass->value();
  rpb.framebuffer = framebuffer;
  rpb.renderArea.offset.x = viewport_region[0];
  rpb.renderArea.offset.y = viewport_region[1];
  rpb.renderArea.extent.width = viewport_region[2] - viewport_region[0];
  rpb.renderArea.extent.height = viewport_region[3] - viewport_region[1];
  rpb.clearValueCount = clear_values.size();
  rpb.pClearValues = clear_values.data();

  vkCmdBeginRenderPass(command_buffer, &rpb, VK_SUBPASS_CONTENTS_INLINE);
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
  vkCmdEndRenderPass(command_buffer);

  auto resources = std::make_tuple(
      vulkan_triangle_soup_, std::move(uniform_descriptor_set),
      std::move(descriptor_pool), std::move(uniform_buffer_memory),
      std::move(uniform_buffer), data_.pipeline, data_.render_pass);
  // std::any doesn't support move only types, so we wrap it in a shared_ptr.
  return FrameResources(
      std::make_shared<decltype(resources)>(std::move(resources)));
}

std::optional<MeshRenderer::Error> MeshRenderer::SetTriangleSoup(
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
