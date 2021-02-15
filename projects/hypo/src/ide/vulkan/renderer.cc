#include "renderer.h"

#include <fmt/format.h>

#include <glm/glm.hpp>
#include <glm/gtc/matrix_transform.hpp>
#include <iostream>

namespace {

#include "src_gen/color_frag.h"
#include "src_gen/color_vert.h"

#define ASSIGN_OR_RETURN(lhs, rhs)                 \
  auto maybe_##lhs = rhs;                          \
  if (auto error = std::get_if<0>(&maybe_##lhs)) { \
    return *error;                                 \
  }                                                \
  auto& lhs = std::get<1>(maybe_##lhs)

// Note that the vertex data and the projection matrix assume OpenGL. With
// Vulkan Y is negated in clip space and the near/far plane is at 0/1 instead
// of -1/1. These will be corrected for by an extra transformation when
// calculating the modelview-projection matrix.
const float VERTEX_DATA[] = {  // Y up, front = CCW
    0.0f, 0.5f, 1.0f, 0.0f,  0.0f, -0.5f, -0.5f, 0.0f,
    1.0f, 0.0f, 0.5f, -0.5f, 0.0f, 0.0f,  1.0f};

struct UniformType {
  alignas(16) glm::mat4 matrix;
};

Renderer::ErrorOr<uint32_t> FindMemoryTypeIndex(
    VkPhysicalDevice physical_device, uint32_t type_filter,
    VkMemoryPropertyFlags properties) {
  VkPhysicalDeviceMemoryProperties mem_properties;
  vkGetPhysicalDeviceMemoryProperties(physical_device, &mem_properties);

  for (uint32_t i = 0; i < mem_properties.memoryTypeCount; i++) {
    if ((type_filter & (1 << i)) &&
        (mem_properties.memoryTypes[i].propertyFlags & properties) ==
            properties) {
      return i;
    }
  }

  return Renderer::Error{"Failed to find a suitable memory type."};
}

Renderer::ErrorOr<WithDeleter<VkShaderModule>> CreateShader(VkDevice device,
                                                            const uint8_t* data,
                                                            size_t size) {
  VkShaderModuleCreateInfo shader_module_create_info;
  memset(&shader_module_create_info, 0, sizeof(shader_module_create_info));
  shader_module_create_info.sType = VK_STRUCTURE_TYPE_SHADER_MODULE_CREATE_INFO;
  shader_module_create_info.codeSize = size;
  shader_module_create_info.pCode = reinterpret_cast<const uint32_t*>(data);
  VkShaderModule shader_module;
  VkResult err = vkCreateShaderModule(device, &shader_module_create_info,
                                      nullptr, &shader_module);
  if (err != VK_SUCCESS) {
    return Renderer::Error{
        fmt::format("Failed to create shader module: {}", err)};
  }

  return WithDeleter<VkShaderModule>(
      std::move(shader_module), [device](VkShaderModule&& x) {
        vkDestroyShaderModule(device, x, nullptr);
      });
}

Renderer::ErrorOr<WithDeleter<VkDescriptorPool>> MakeDescriptorPool(
    VkDevice device,
    const std::vector<VkDescriptorPoolSize> descriptor_pool_sizes,
    uint32_t max_sets) {
  VkDescriptorPoolCreateInfo descriptor_pool_create_info;
  memset(&descriptor_pool_create_info, 0, sizeof(descriptor_pool_create_info));
  descriptor_pool_create_info.sType =
      VK_STRUCTURE_TYPE_DESCRIPTOR_POOL_CREATE_INFO;
  descriptor_pool_create_info.maxSets = max_sets;
  descriptor_pool_create_info.poolSizeCount = descriptor_pool_sizes.size();
  descriptor_pool_create_info.pPoolSizes = descriptor_pool_sizes.data();

  VkDescriptorPool pool;
  auto err = vkCreateDescriptorPool(device, &descriptor_pool_create_info,
                                    nullptr, &pool);
  if (err != VK_SUCCESS) {
    return Renderer::Error{
        fmt::format("Failed to create descriptor pool: {}", err)};
  } else {
    return WithDeleter<VkDescriptorPool>(
        std::move(pool), [device](VkDescriptorPool&& x) {
          vkDestroyDescriptorPool(device, x, nullptr);
        });
  }
}

Renderer::ErrorOr<WithDeleter<VkDescriptorSetLayout>> MakeDescriptorSetLayout(
    VkDevice device,
    std::vector<VkDescriptorSetLayoutBinding> layout_bindings) {
  VkDescriptorSetLayoutCreateInfo descLayoutInfo = {
      VK_STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_CREATE_INFO, nullptr, 0,
      static_cast<uint32_t>(layout_bindings.size()), layout_bindings.data()};
  VkDescriptorSetLayout layout;
  auto err =
      vkCreateDescriptorSetLayout(device, &descLayoutInfo, nullptr, &layout);
  if (err != VK_SUCCESS) {
    return Renderer::Error{
        fmt::format("Failed to create descriptor set layout: {}", err)};
  } else {
    return WithDeleter<VkDescriptorSetLayout>(
        std::move(layout), [device](VkDescriptorSetLayout&& x) {
          vkDestroyDescriptorSetLayout(device, x, nullptr);
        });
  }
}

Renderer::ErrorOr<WithDeleter<VkDescriptorSet>> MakeDescriptorSet(
    VkDevice device, VkDescriptorPool pool, const VkDescriptorSetLayout& layout,
    const std::vector<
        std::variant<VkDescriptorBufferInfo, VkDescriptorImageInfo>>&
        descriptor_set_write_infos) {
  VkDescriptorSetAllocateInfo alloc_info{};
  alloc_info.sType = VK_STRUCTURE_TYPE_DESCRIPTOR_SET_ALLOCATE_INFO;
  alloc_info.descriptorPool = pool;
  alloc_info.descriptorSetCount = 1;
  alloc_info.pSetLayouts = &layout;

  VkDescriptorSet descriptor_set;
  auto err = vkAllocateDescriptorSets(device, &alloc_info, &descriptor_set);
  if (err != VK_SUCCESS) {
    return Renderer::Error{
        fmt::format("Failed to create descriptor set: {}", err)};
  }

  std::vector<VkWriteDescriptorSet> writes;
  writes.reserve(descriptor_set_write_infos.size());
  for (uint32_t i = 0; i < descriptor_set_write_infos.size(); ++i) {
    VkWriteDescriptorSet write_descriptor_set;
    write_descriptor_set.sType = VK_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET;
    write_descriptor_set.dstSet = descriptor_set;
    write_descriptor_set.dstBinding = i;
    write_descriptor_set.dstArrayElement = 0;
    write_descriptor_set.descriptorCount = 1;

    if (auto buffer_info = std::get_if<VkDescriptorBufferInfo>(
            &descriptor_set_write_infos[i])) {
      write_descriptor_set.descriptorType = VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER;
      write_descriptor_set.pBufferInfo = buffer_info;
    } else if (auto image_info = std::get_if<VkDescriptorImageInfo>(
                   &descriptor_set_write_infos[i])) {
      write_descriptor_set.descriptorType =
          VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER;
      write_descriptor_set.pImageInfo = image_info;
    }

    writes.push_back(write_descriptor_set);
  }

  vkUpdateDescriptorSets(device, writes.size(), writes.data(), 0, nullptr);

  return WithDeleter<VkDescriptorSet>(
      std::move(descriptor_set), [device, pool](VkDescriptorSet&& x) {
        vkFreeDescriptorSets(device, pool, 1, &x);
      });
}

Renderer::ErrorOr<WithDeleter<VkDeviceMemory>> Allocate(VkDevice device,
                                                        VkDeviceSize size,
                                                        uint32_t memory_type) {
  VkMemoryAllocateInfo mem_alloc_info = {VK_STRUCTURE_TYPE_MEMORY_ALLOCATE_INFO,
                                         nullptr, size, memory_type};

  VkDeviceMemory vk_memory;
  auto err = vkAllocateMemory(device, &mem_alloc_info, nullptr, &vk_memory);
  if (err != VK_SUCCESS) {
    return Renderer::Error{
        fmt::format("Failed to allocate {} bytes of memory: {}", size, err)};
  }

  return WithDeleter<VkDeviceMemory>(
      std::move(vk_memory),
      [device](VkDeviceMemory&& x) { vkFreeMemory(device, x, nullptr); });
}

Renderer::ErrorOr<WithDeleter<VkDeviceMemory>> Allocate(
    VkPhysicalDevice physical_device, VkDevice device, VkDeviceSize size,
    uint32_t type_filter, VkMemoryPropertyFlags memory_property_flags) {
  ASSIGN_OR_RETURN(
      memory_type,
      FindMemoryTypeIndex(physical_device, type_filter, memory_property_flags));

  return Allocate(device, size, memory_type);
}

Renderer::ErrorOr<WithDeleter<VkDeviceMemory>> Allocate(
    VkPhysicalDevice physical_device, VkDevice device, VkBuffer buffer,
    VkMemoryPropertyFlags memory_property_flags) {
  VkMemoryRequirements mem_reqs;
  vkGetBufferMemoryRequirements(device, buffer, &mem_reqs);

  return Allocate(physical_device, device, mem_reqs.size,
                  mem_reqs.memoryTypeBits, memory_property_flags);
}

Renderer::ErrorOr<WithDeleter<VkBuffer>> MakeBuffer(
    VkDevice device, VkBufferUsageFlagBits usage_flags, size_t data_size) {
  VkBufferCreateInfo vertex_buffer_create_info{};
  vertex_buffer_create_info.sType = VK_STRUCTURE_TYPE_BUFFER_CREATE_INFO;
  vertex_buffer_create_info.size = data_size;
  vertex_buffer_create_info.usage = usage_flags;

  VkBuffer buffer;
  VkResult err =
      vkCreateBuffer(device, &vertex_buffer_create_info, nullptr, &buffer);
  if (err != VK_SUCCESS) {
    return Renderer::Error{fmt::format("Failed to create buffer: {}", err)};
  }
  return WithDeleter<VkBuffer>(std::move(buffer), [device](VkBuffer&& x) {
    vkDestroyBuffer(device, x, nullptr);
  });
}

Renderer::ErrorOr<WithDeleter<VkDeviceMemory>> AllocateAndBindBufferMemory(
    VkPhysicalDevice physical_device, VkDevice device, VkBuffer buffer,
    const uint8_t* data, size_t data_size) {
  ASSIGN_OR_RETURN(buffer_mem,
                   Allocate(physical_device, device, buffer,
                            VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT |
                                VK_MEMORY_PROPERTY_HOST_COHERENT_BIT));

  VkResult err = vkBindBufferMemory(device, buffer, buffer_mem.value(), 0);
  if (err != VK_SUCCESS) {
    return Renderer::Error{
        fmt::format("Failed to bind buffer memory: {}", err)};
  }

  uint8_t* p;
  err = vkMapMemory(device, buffer_mem.value(), 0, data_size, 0,
                    reinterpret_cast<void**>(&p));
  if (err != VK_SUCCESS) {
    return Renderer::Error{fmt::format("Failed to map memory: {}", err)};
  }

  memcpy(p, data, data_size);

  vkUnmapMemory(device, buffer_mem.value());

  return std::move(buffer_mem);
}

Renderer::ErrorOr<WithDeleter<VkPipelineCache>> MakePipelineCache(
    VkDevice device) {
  VkPipelineCache pipeline_cache;
  VkPipelineCacheCreateInfo pipeline_cache_info{};
  pipeline_cache_info.sType = VK_STRUCTURE_TYPE_PIPELINE_CACHE_CREATE_INFO;
  auto err = vkCreatePipelineCache(device, &pipeline_cache_info, nullptr,
                                   &pipeline_cache);
  if (err != VK_SUCCESS) {
    return Renderer::Error{
        fmt::format("Failed to create pipeline cache: {}\n", err)};
  }
  return WithDeleter<VkPipelineCache>(
      std::move(pipeline_cache), [device](VkPipelineCache&& x) {
        vkDestroyPipelineCache(device, x, nullptr);
      });
}

Renderer::ErrorOr<WithDeleter<VkPipelineLayout>> MakePipelineLayout(
    VkDevice device, VkDescriptorSetLayout descriptor_set_layout) {
  VkPipelineLayoutCreateInfo pipeline_layout_create_info{};
  pipeline_layout_create_info.sType =
      VK_STRUCTURE_TYPE_PIPELINE_LAYOUT_CREATE_INFO;
  pipeline_layout_create_info.setLayoutCount = 1;
  pipeline_layout_create_info.pSetLayouts = &descriptor_set_layout;

  VkPipelineLayout pipeline_layout;
  auto err = vkCreatePipelineLayout(device, &pipeline_layout_create_info,
                                    nullptr, &pipeline_layout);
  if (err != VK_SUCCESS) {
    return Renderer::Error{
        fmt::format("Failed to create pipeline layout: {}", err)};
  }

  return WithDeleter<VkPipelineLayout>(
      std::move(pipeline_layout), [device](VkPipelineLayout&& x) {
        vkDestroyPipelineLayout(device, x, nullptr);
      });
}

Renderer::ErrorOr<WithDeleter<VkRenderPass>> MakeRenderPass(
    VkDevice device, VkFormat color_attachment_format) {
  VkAttachmentDescription color_attachment{};
  color_attachment.format = color_attachment_format;
  color_attachment.samples = VK_SAMPLE_COUNT_1_BIT;
  color_attachment.loadOp = VK_ATTACHMENT_LOAD_OP_DONT_CARE;
  color_attachment.storeOp = VK_ATTACHMENT_STORE_OP_STORE;

  color_attachment.initialLayout = VK_IMAGE_LAYOUT_UNDEFINED;
  color_attachment.finalLayout = VK_IMAGE_LAYOUT_PRESENT_SRC_KHR;

  VkAttachmentReference color_attachment_ref{};
  color_attachment_ref.attachment = 0;
  color_attachment_ref.layout = VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL;

  VkSubpassDescription subpass{};
  subpass.pipelineBindPoint = VK_PIPELINE_BIND_POINT_GRAPHICS;
  subpass.colorAttachmentCount = 1;
  subpass.pColorAttachments = &color_attachment_ref;

  VkRenderPassCreateInfo render_pass_info{};
  render_pass_info.sType = VK_STRUCTURE_TYPE_RENDER_PASS_CREATE_INFO;
  render_pass_info.attachmentCount = 1;
  render_pass_info.pAttachments = &color_attachment;
  render_pass_info.subpassCount = 1;
  render_pass_info.pSubpasses = &subpass;

  VkRenderPass render_pass;
  auto err =
      vkCreateRenderPass(device, &render_pass_info, nullptr, &render_pass);
  if (err != VK_SUCCESS) {
    return Renderer::Error{
        fmt::format("Failed to create render pass: {}", err)};
  }

  return WithDeleter<VkRenderPass>(
      std::move(render_pass),
      [device](VkRenderPass&& x) { vkDestroyRenderPass(device, x, nullptr); });
}

Renderer::ErrorOr<WithDeleter<VkPipeline>> MakePipeline(
    VkDevice device, VkPipelineLayout pipeline_layout, VkRenderPass render_pass,
    VkPipelineCache pipeline_cache, VkShaderModule vertex_shader_module,
    const std::vector<VkVertexInputBindingDescription>&
        vertex_input_binding_descriptions,
    const std::vector<VkVertexInputAttributeDescription>&
        vertex_input_attribute_description,
    VkShaderModule fragment_shader_module) {
  VkGraphicsPipelineCreateInfo pipeline_create_info;
  memset(&pipeline_create_info, 0, sizeof(pipeline_create_info));
  pipeline_create_info.sType = VK_STRUCTURE_TYPE_GRAPHICS_PIPELINE_CREATE_INFO;

  VkPipelineShaderStageCreateInfo shader_stages[2] = {
      {VK_STRUCTURE_TYPE_PIPELINE_SHADER_STAGE_CREATE_INFO, nullptr, 0,
       VK_SHADER_STAGE_VERTEX_BIT, vertex_shader_module, "main", nullptr},
      {VK_STRUCTURE_TYPE_PIPELINE_SHADER_STAGE_CREATE_INFO, nullptr, 0,
       VK_SHADER_STAGE_FRAGMENT_BIT, fragment_shader_module, "main", nullptr}};
  pipeline_create_info.stageCount = 2;
  pipeline_create_info.pStages = shader_stages;

  VkPipelineVertexInputStateCreateInfo vertex_input_info;
  vertex_input_info.sType =
      VK_STRUCTURE_TYPE_PIPELINE_VERTEX_INPUT_STATE_CREATE_INFO;
  vertex_input_info.pNext = nullptr;
  vertex_input_info.flags = 0;
  vertex_input_info.vertexBindingDescriptionCount =
      vertex_input_binding_descriptions.size();
  vertex_input_info.pVertexBindingDescriptions =
      vertex_input_binding_descriptions.data();
  vertex_input_info.vertexAttributeDescriptionCount =
      vertex_input_attribute_description.size();
  vertex_input_info.pVertexAttributeDescriptions =
      vertex_input_attribute_description.data();

  pipeline_create_info.pVertexInputState = &vertex_input_info;

  VkPipelineInputAssemblyStateCreateInfo ia;
  memset(&ia, 0, sizeof(ia));
  ia.sType = VK_STRUCTURE_TYPE_PIPELINE_INPUT_ASSEMBLY_STATE_CREATE_INFO;
  ia.topology = VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST;
  pipeline_create_info.pInputAssemblyState = &ia;

  // The viewport and scissor will be set dynamically via
  // vkCmdSetViewport/Scissor. This way the pipeline does not need to be
  // touched when resizing the window.
  VkPipelineViewportStateCreateInfo vp;
  memset(&vp, 0, sizeof(vp));
  vp.sType = VK_STRUCTURE_TYPE_PIPELINE_VIEWPORT_STATE_CREATE_INFO;
  vp.viewportCount = 1;
  vp.scissorCount = 1;
  pipeline_create_info.pViewportState = &vp;

  VkPipelineRasterizationStateCreateInfo rs;
  memset(&rs, 0, sizeof(rs));
  rs.sType = VK_STRUCTURE_TYPE_PIPELINE_RASTERIZATION_STATE_CREATE_INFO;
  rs.polygonMode = VK_POLYGON_MODE_FILL;
  rs.cullMode = VK_CULL_MODE_NONE;  // we want the back face as well
  rs.frontFace = VK_FRONT_FACE_COUNTER_CLOCKWISE;
  rs.lineWidth = 1.0f;
  pipeline_create_info.pRasterizationState = &rs;

  VkPipelineMultisampleStateCreateInfo ms;
  memset(&ms, 0, sizeof(ms));
  ms.sType = VK_STRUCTURE_TYPE_PIPELINE_MULTISAMPLE_STATE_CREATE_INFO;
  // Enable multisampling.
  ms.rasterizationSamples = VK_SAMPLE_COUNT_1_BIT;
  pipeline_create_info.pMultisampleState = &ms;

  VkPipelineDepthStencilStateCreateInfo ds;
  memset(&ds, 0, sizeof(ds));
  ds.sType = VK_STRUCTURE_TYPE_PIPELINE_DEPTH_STENCIL_STATE_CREATE_INFO;
  ds.depthTestEnable = VK_TRUE;
  ds.depthWriteEnable = VK_TRUE;
  ds.depthCompareOp = VK_COMPARE_OP_LESS_OR_EQUAL;
  pipeline_create_info.pDepthStencilState = &ds;

  VkPipelineColorBlendStateCreateInfo cb;
  memset(&cb, 0, sizeof(cb));
  cb.sType = VK_STRUCTURE_TYPE_PIPELINE_COLOR_BLEND_STATE_CREATE_INFO;
  // no blend, write out all of rgba
  VkPipelineColorBlendAttachmentState att;
  memset(&att, 0, sizeof(att));
  att.colorWriteMask = 0xF;
  cb.attachmentCount = 1;
  cb.pAttachments = &att;
  pipeline_create_info.pColorBlendState = &cb;

  VkDynamicState dynamic_states[] = {VK_DYNAMIC_STATE_VIEWPORT,
                                     VK_DYNAMIC_STATE_SCISSOR};
  VkPipelineDynamicStateCreateInfo dyn;
  memset(&dyn, 0, sizeof(dyn));
  dyn.sType = VK_STRUCTURE_TYPE_PIPELINE_DYNAMIC_STATE_CREATE_INFO;
  dyn.dynamicStateCount = sizeof(dynamic_states) / sizeof(VkDynamicState);
  dyn.pDynamicStates = dynamic_states;
  pipeline_create_info.pDynamicState = &dyn;

  pipeline_create_info.layout = pipeline_layout;
  pipeline_create_info.renderPass = render_pass;

  VkPipeline pipeline;
  auto err = vkCreateGraphicsPipelines(
      device, pipeline_cache, 1, &pipeline_create_info, nullptr, &pipeline);
  if (err != VK_SUCCESS) {
    return Renderer::Error{
        fmt::format("Failed to create graphics pipeline: {}", err)};
  }

  return WithDeleter<VkPipeline>(std::move(pipeline), [device](VkPipeline&& x) {
    vkDestroyPipeline(device, x, nullptr);
  });
}

}  // namespace

// static
Renderer::ErrorOr<Renderer> Renderer::Create(VkInstance instance,
                                             VkPhysicalDevice physical_device,
                                             VkDevice device,
                                             VkFormat output_image_format) {
  ASSIGN_OR_RETURN(vertex_buffer,
                   MakeBuffer(device, VK_BUFFER_USAGE_VERTEX_BUFFER_BIT,
                              sizeof(VERTEX_DATA)));
  ASSIGN_OR_RETURN(
      vertex_buffer_memory,
      AllocateAndBindBufferMemory(
          physical_device, device, vertex_buffer.value(),
          reinterpret_cast<const uint8_t*>(VERTEX_DATA), sizeof(VERTEX_DATA)));

  // Set up descriptor set and its layout.
  ASSIGN_OR_RETURN(
      descriptor_pool,
      MakeDescriptorPool(
          device, {VkDescriptorPoolSize{VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER, 1}},
          1));

  ASSIGN_OR_RETURN(descriptor_set_layout,
                   MakeDescriptorSetLayout(
                       device, {VkDescriptorSetLayoutBinding{
                                   0, VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER, 1,
                                   VK_SHADER_STAGE_VERTEX_BIT, nullptr}}));

  // Pipeline cache
  ASSIGN_OR_RETURN(pipeline_cache, MakePipelineCache(device));

  // Pipeline layout
  ASSIGN_OR_RETURN(pipeline_layout,
                   MakePipelineLayout(device, descriptor_set_layout.value()));

  // Shaders
  ASSIGN_OR_RETURN(vertex_shader_module,
                   CreateShader(device, color_vert_spv, color_vert_spv_len));
  ASSIGN_OR_RETURN(fragment_shader_module,
                   CreateShader(device, color_frag_spv, color_frag_spv_len));

  // Render pass
  ASSIGN_OR_RETURN(render_pass, MakeRenderPass(device, output_image_format));

  // Graphics pipeline
  ASSIGN_OR_RETURN(
      pipeline,
      MakePipeline(device, pipeline_layout.value(), render_pass.value(),
                   pipeline_cache.value(), vertex_shader_module.value(),
                   {
                       {
                           0,  // binding
                           5 * sizeof(float),
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
                       {
                           // color
                           1,
                           0,
                           VK_FORMAT_R32G32B32_SFLOAT,
                           2 * sizeof(float),
                       },
                   },
                   fragment_shader_module.value()));

  return Renderer(RendererConstructorData{
      instance,
      physical_device,
      device,
      std::move(vertex_buffer),
      std::move(vertex_buffer_memory),
      std::move(descriptor_pool),
      std::move(descriptor_set_layout),
      std::move(pipeline_cache),
      std::move(pipeline_layout),
      std::move(render_pass),
      std::move(pipeline),
  });
}

Renderer::~Renderer() {}

glm::mat4 PerspectiveMatrix(float vertical_fov, float aspect_ratio,
                            float near_plane, float far_plane) {
  const float z_range = near_plane - far_plane;
  const float tan_half_fov = glm::tan(glm::radians(vertical_fov / 2.0));

  // Column major.
  return glm::mat4{
      {1.0f / (tan_half_fov * aspect_ratio), 0.0f, 0.0f, 0.0f},
      {0.0f, 1.0f / tan_half_fov, 0.0f, 0.0f},
      {0.0f, 0.0f, (-near_plane - far_plane) / z_range, 1.0f},
      {0.0f, 0.0f, 2.0f * far_plane * near_plane / z_range, 0.0f},
  };
}

auto Renderer::RenderFrame(VkCommandBuffer command_buffer,
                           VkFramebuffer framebuffer,
                           const std::array<uint32_t, 2>& output_surface_size)
    -> ErrorOr<FrameResources> {
  glm::mat4 projection_matrix = PerspectiveMatrix(
      45.0f, output_surface_size[0] / (float)output_surface_size[1], 0.01f,
      100.0f);
  glm::mat4 view_matrix =
      glm::translate(glm::mat4(), glm::vec3(0.0f, 0.0f, -4.0f));
  glm::mat4 world_matrix =
      glm::rotate(glm::mat4(), rotation_, glm::vec3(0.0f, 1.0f, 0.0f));
  UniformType uniform_data{projection_matrix * view_matrix * world_matrix};

  rotation_ += 1;

  ASSIGN_OR_RETURN(uniform_buffer,
                   MakeBuffer(data_.device, VK_BUFFER_USAGE_UNIFORM_BUFFER_BIT,
                              sizeof(uniform_data)));
  ASSIGN_OR_RETURN(
      uniform_buffer_memory,
      AllocateAndBindBufferMemory(
          data_.physical_device, data_.device, uniform_buffer.value(),
          reinterpret_cast<uint8_t*>(&uniform_data), sizeof(uniform_data)));

  ASSIGN_OR_RETURN(
      uniform_descriptor_set,
      MakeDescriptorSet(data_.device, data_.descriptor_pool.value(),
                        data_.descriptor_set_layout.value(),
                        {VkDescriptorBufferInfo{uniform_buffer.value(), 0,
                                                sizeof(UniformType)}}));

  VkClearColorValue clear_color = {{0, 0, 0, 1}};
  VkClearDepthStencilValue clear_depth_stencil = {1, 0};
  std::array<VkClearValue, 2> clear_values{};
  clear_values[0].color = clear_values[2].color = clear_color;
  clear_values[1].depthStencil = clear_depth_stencil;

  VkRenderPassBeginInfo rpb{};
  rpb.sType = VK_STRUCTURE_TYPE_RENDER_PASS_BEGIN_INFO;
  rpb.renderPass = data_.render_pass.value();
  rpb.framebuffer = framebuffer;
  rpb.renderArea.extent.width = output_surface_size[0];
  rpb.renderArea.extent.height = output_surface_size[1];
  rpb.clearValueCount = clear_values.size();
  rpb.pClearValues = clear_values.data();
  vkCmdBeginRenderPass(command_buffer, &rpb, VK_SUBPASS_CONTENTS_INLINE);

  vkCmdBindPipeline(command_buffer, VK_PIPELINE_BIND_POINT_GRAPHICS,
                    data_.pipeline.value());
  vkCmdBindDescriptorSets(command_buffer, VK_PIPELINE_BIND_POINT_GRAPHICS,
                          data_.pipeline_layout.value(), 0, 1,
                          &(uniform_descriptor_set.value()), 0, nullptr);

  VkDeviceSize vertex_buffer_offset = 0;
  vkCmdBindVertexBuffers(command_buffer, 0, 1, &(data_.vertex_buffer.value()),
                         &vertex_buffer_offset);

  VkViewport viewport;
  viewport.x = viewport.y = 0;
  viewport.width = output_surface_size[0];
  viewport.height = output_surface_size[1];
  viewport.minDepth = 0;
  viewport.maxDepth = 1;
  vkCmdSetViewport(command_buffer, 0, 1, &viewport);

  VkRect2D scissor;
  scissor.offset.x = scissor.offset.y = 0;
  scissor.extent.width = viewport.width;
  scissor.extent.height = viewport.height;
  vkCmdSetScissor(command_buffer, 0, 1, &scissor);

  vkCmdDraw(command_buffer, 3, 1, 0, 0);

  vkCmdEndRenderPass(command_buffer);

  auto resources = std::make_tuple(std::move(uniform_buffer),
                                   std::move(uniform_buffer_memory),
                                   std::move(uniform_descriptor_set));
  // std::any doesn't support move only types, so we wrap it in a shared_ptr.
  return FrameResources(
      std::make_shared<decltype(resources)>(std::move(resources)));
}
