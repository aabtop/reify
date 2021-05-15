#include "vulkan_utils/vulkan_utils.h"

#include <fmt/format.h>
#include <vulkan/vulkan.h>

#include <array>
#include <optional>

namespace vulkan_utils {

ErrorOr<uint32_t> FindMemoryTypeIndex(VkPhysicalDevice physical_device,
                                      uint32_t type_filter,
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

  return Error{"Failed to find a suitable memory type."};
}

ErrorOr<WithDeleter<VkShaderModule>> CreateShader(VkDevice device,
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
    return Error{fmt::format("Failed to create shader module: {}", err)};
  }

  return WithDeleter<VkShaderModule>(
      std::move(shader_module), [device](VkShaderModule&& x) {
        vkDestroyShaderModule(device, x, nullptr);
      });
}

ErrorOr<WithDeleter<VkDescriptorPool>> MakeDescriptorPool(
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
    return Error{fmt::format("Failed to create descriptor pool: {}", err)};
  } else {
    return WithDeleter<VkDescriptorPool>(
        std::move(pool), [device](VkDescriptorPool&& x) {
          vkDestroyDescriptorPool(device, x, nullptr);
        });
  }
}

ErrorOr<WithDeleter<VkDescriptorSetLayout>> MakeDescriptorSetLayout(
    VkDevice device,
    std::vector<VkDescriptorSetLayoutBinding> layout_bindings) {
  VkDescriptorSetLayoutCreateInfo descLayoutInfo = {
      VK_STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_CREATE_INFO, nullptr, 0,
      static_cast<uint32_t>(layout_bindings.size()), layout_bindings.data()};
  VkDescriptorSetLayout layout;
  auto err =
      vkCreateDescriptorSetLayout(device, &descLayoutInfo, nullptr, &layout);
  if (err != VK_SUCCESS) {
    return Error{
        fmt::format("Failed to create descriptor set layout: {}", err)};
  } else {
    return WithDeleter<VkDescriptorSetLayout>(
        std::move(layout), [device](VkDescriptorSetLayout&& x) {
          vkDestroyDescriptorSetLayout(device, x, nullptr);
        });
  }
}

ErrorOr<WithDeleter<VkDescriptorSet>> MakeDescriptorSet(
    VkDevice device, VkDescriptorPool pool, VkDescriptorSetLayout layout,
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
    return Error{fmt::format("Failed to create descriptor set: {}", err)};
  }

  std::vector<VkWriteDescriptorSet> writes;
  writes.reserve(descriptor_set_write_infos.size());
  for (uint32_t i = 0; i < descriptor_set_write_infos.size(); ++i) {
    VkWriteDescriptorSet write_descriptor_set{};
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

ErrorOr<WithDeleter<VkDeviceMemory>> Allocate(VkDevice device,
                                              VkDeviceSize size,
                                              uint32_t memory_type) {
  VkMemoryAllocateInfo mem_alloc_info = {VK_STRUCTURE_TYPE_MEMORY_ALLOCATE_INFO,
                                         nullptr, size, memory_type};

  VkDeviceMemory vk_memory;
  auto err = vkAllocateMemory(device, &mem_alloc_info, nullptr, &vk_memory);
  if (err != VK_SUCCESS) {
    return Error{
        fmt::format("Failed to allocate {} bytes of memory: {}", size, err)};
  }

  return WithDeleter<VkDeviceMemory>(
      std::move(vk_memory),
      [device](VkDeviceMemory&& x) { vkFreeMemory(device, x, nullptr); });
}

ErrorOr<WithDeleter<VkDeviceMemory>> Allocate(
    VkPhysicalDevice physical_device, VkDevice device, VkDeviceSize size,
    uint32_t type_filter, VkMemoryPropertyFlags memory_property_flags) {
  VULKAN_UTILS_ASSIGN_OR_RETURN(
      memory_type,
      FindMemoryTypeIndex(physical_device, type_filter, memory_property_flags));

  return Allocate(device, size, memory_type);
}

ErrorOr<WithDeleter<VkBuffer>> MakeBuffer(VkDevice device,
                                          VkBufferUsageFlagBits usage_flags,
                                          size_t data_size) {
  VkBufferCreateInfo vertex_buffer_create_info{};
  vertex_buffer_create_info.sType = VK_STRUCTURE_TYPE_BUFFER_CREATE_INFO;
  vertex_buffer_create_info.size = data_size;
  vertex_buffer_create_info.usage = usage_flags;

  VkBuffer buffer;
  VkResult err =
      vkCreateBuffer(device, &vertex_buffer_create_info, nullptr, &buffer);
  if (err != VK_SUCCESS) {
    return Error{fmt::format("Failed to create buffer: {}", err)};
  }
  return WithDeleter<VkBuffer>(std::move(buffer), [device](VkBuffer&& x) {
    vkDestroyBuffer(device, x, nullptr);
  });
}

ErrorOr<WithDeleter<VkDeviceMemory>> AllocateAndBindBufferMemory(
    VkPhysicalDevice physical_device, VkDevice device, VkBuffer buffer,
    const uint8_t* data, size_t data_size) {
  VkMemoryRequirements mem_reqs;
  vkGetBufferMemoryRequirements(device, buffer, &mem_reqs);

  VULKAN_UTILS_ASSIGN_OR_RETURN(
      buffer_mem,
      Allocate(physical_device, device, mem_reqs.size, mem_reqs.memoryTypeBits,
               VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT |
                   VK_MEMORY_PROPERTY_HOST_COHERENT_BIT));

  VkResult err = vkBindBufferMemory(device, buffer, buffer_mem.value(), 0);
  if (err != VK_SUCCESS) {
    return Error{fmt::format("Failed to bind buffer memory: {}", err)};
  }

  uint8_t* p;
  err = vkMapMemory(device, buffer_mem.value(), 0, data_size, 0,
                    reinterpret_cast<void**>(&p));
  if (err != VK_SUCCESS) {
    return Error{fmt::format("Failed to map memory: {}", err)};
  }

  memcpy(p, data, data_size);

  vkUnmapMemory(device, buffer_mem.value());

  return std::move(buffer_mem);
}

ErrorOr<WithDeleter<VkPipelineCache>> MakePipelineCache(VkDevice device) {
  VkPipelineCache pipeline_cache;
  VkPipelineCacheCreateInfo pipeline_cache_info{};
  pipeline_cache_info.sType = VK_STRUCTURE_TYPE_PIPELINE_CACHE_CREATE_INFO;
  auto err = vkCreatePipelineCache(device, &pipeline_cache_info, nullptr,
                                   &pipeline_cache);
  if (err != VK_SUCCESS) {
    return Error{fmt::format("Failed to create pipeline cache: {}\n", err)};
  }
  return WithDeleter<VkPipelineCache>(
      std::move(pipeline_cache), [device](VkPipelineCache&& x) {
        vkDestroyPipelineCache(device, x, nullptr);
      });
}

ErrorOr<WithDeleter<VkPipelineLayout>> MakePipelineLayout(
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
    return Error{fmt::format("Failed to create pipeline layout: {}", err)};
  }

  return WithDeleter<VkPipelineLayout>(
      std::move(pipeline_layout), [device](VkPipelineLayout&& x) {
        vkDestroyPipelineLayout(device, x, nullptr);
      });
}

ErrorOr<WithDeleter<VkImage>> MakeImage(VkDevice device, uint32_t width,
                                        uint32_t height, VkFormat format,
                                        VkImageTiling tiling,
                                        VkImageUsageFlags usage) {
  VkImageCreateInfo image_info{};
  image_info.sType = VK_STRUCTURE_TYPE_IMAGE_CREATE_INFO;
  image_info.imageType = VK_IMAGE_TYPE_2D;
  image_info.extent.width = width;
  image_info.extent.height = height;
  image_info.extent.depth = 1;
  image_info.mipLevels = 1;
  image_info.arrayLayers = 1;
  image_info.format = format;
  image_info.tiling = tiling;
  image_info.initialLayout = VK_IMAGE_LAYOUT_UNDEFINED;
  image_info.usage = usage;
  image_info.samples = VK_SAMPLE_COUNT_1_BIT;
  image_info.sharingMode = VK_SHARING_MODE_EXCLUSIVE;

  VkImage image;
  VkResult err = vkCreateImage(device, &image_info, nullptr, &image);
  if (err != VK_SUCCESS) {
    return Error{fmt::format("Failed to create image: {}", err)};
  }

  return WithDeleter<VkImage>(std::move(image), [device](VkImage&& x) {
    vkDestroyImage(device, x, nullptr);
  });
}

ErrorOr<WithDeleter<VkDeviceMemory>> AllocateAndBindImageMemory(
    VkPhysicalDevice physical_device, VkDevice device, VkImage image,
    VkMemoryPropertyFlags properties) {
  VkMemoryRequirements mem_requirements;
  vkGetImageMemoryRequirements(device, image, &mem_requirements);

  VULKAN_UTILS_ASSIGN_OR_RETURN(
      image_memory, Allocate(physical_device, device, mem_requirements.size,
                             mem_requirements.memoryTypeBits, properties));

  VkResult err = vkBindImageMemory(device, image, image_memory.value(), 0);
  if (err != VK_SUCCESS) {
    return Error{fmt::format("Error binding image memory: {}", err)};
  }

  return std::move(image_memory);
}

ErrorOr<WithDeleter<VkImageView>> MakeImageView(VkDevice device, VkImage image,
                                                VkFormat format) {
  VkImageViewCreateInfo view_info{};
  view_info.sType = VK_STRUCTURE_TYPE_IMAGE_VIEW_CREATE_INFO;
  view_info.image = image;
  view_info.viewType = VK_IMAGE_VIEW_TYPE_2D;
  view_info.format = format;
  view_info.subresourceRange.aspectMask = VK_IMAGE_ASPECT_COLOR_BIT;
  view_info.subresourceRange.baseMipLevel = 0;
  view_info.subresourceRange.levelCount = 1;
  view_info.subresourceRange.baseArrayLayer = 0;
  view_info.subresourceRange.layerCount = 1;

  VkImageView image_view;
  VkResult err = vkCreateImageView(device, &view_info, nullptr, &image_view);
  if (err != VK_SUCCESS) {
    return Error{fmt::format("Error creating image view: {}", err)};
  }

  return WithDeleter<VkImageView>(
      std::move(image_view),
      [device](VkImageView&& x) { vkDestroyImageView(device, x, nullptr); });
}

ErrorOr<WithDeleter<VkRenderPass>> MakeRenderPass(
    VkDevice device,
    const std::vector<VkAttachmentDescription>& color_attachments,
    const std::optional<VkAttachmentDescription>&
        maybe_depth_stencil_attachment) {
  std::vector<VkAttachmentDescription> all_attachments;
  all_attachments.reserve(color_attachments.size() +
                          (maybe_depth_stencil_attachment ? 1 : 0));

  std::vector<VkAttachmentReference> color_attachment_refs;
  color_attachment_refs.reserve(color_attachments.size());
  for (size_t i = 0; i < color_attachments.size(); ++i) {
    color_attachment_refs.push_back(VkAttachmentReference{
        static_cast<uint32_t>(i),
        VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL,
    });
    all_attachments.push_back(color_attachments[i]);
  }
  std::optional<VkAttachmentReference> maybe_depth_stencil_attachment_ref;
  if (maybe_depth_stencil_attachment) {
    maybe_depth_stencil_attachment_ref.emplace(VkAttachmentReference{
        static_cast<uint32_t>(color_attachments.size()),
        VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL});
    all_attachments.push_back(*maybe_depth_stencil_attachment);
  }

  VkSubpassDescription subpass{};
  subpass.pipelineBindPoint = VK_PIPELINE_BIND_POINT_GRAPHICS;
  subpass.colorAttachmentCount =
      static_cast<uint32_t>(color_attachment_refs.size());
  subpass.pColorAttachments = color_attachment_refs.data();
  subpass.pDepthStencilAttachment = maybe_depth_stencil_attachment_ref
                                        ? &(*maybe_depth_stencil_attachment_ref)
                                        : nullptr;

  VkSubpassDependency dependency{};
  dependency.srcSubpass = VK_SUBPASS_EXTERNAL;
  dependency.dstSubpass = 0;
  dependency.srcAccessMask = 0;
  if (!color_attachments.empty()) {
    dependency.srcStageMask |= VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT;
    dependency.dstStageMask |= VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT;
    dependency.dstAccessMask |= VK_ACCESS_COLOR_ATTACHMENT_WRITE_BIT;
  }
  if (maybe_depth_stencil_attachment) {
    dependency.srcStageMask |= VK_PIPELINE_STAGE_EARLY_FRAGMENT_TESTS_BIT;
    dependency.dstStageMask |= VK_PIPELINE_STAGE_EARLY_FRAGMENT_TESTS_BIT;
    dependency.dstAccessMask |= VK_ACCESS_DEPTH_STENCIL_ATTACHMENT_WRITE_BIT;
  }

  VkRenderPassCreateInfo render_pass_info{};
  render_pass_info.sType = VK_STRUCTURE_TYPE_RENDER_PASS_CREATE_INFO;
  render_pass_info.attachmentCount =
      static_cast<uint32_t>(all_attachments.size());
  render_pass_info.pAttachments = all_attachments.data();
  render_pass_info.subpassCount = 1;
  render_pass_info.pSubpasses = &subpass;
  render_pass_info.dependencyCount = 1;
  render_pass_info.pDependencies = &dependency;

  VkRenderPass render_pass;
  auto err =
      vkCreateRenderPass(device, &render_pass_info, nullptr, &render_pass);
  if (err != VK_SUCCESS) {
    return Error{fmt::format("Failed to create render pass: {}", err)};
  }

  return WithDeleter<VkRenderPass>(
      std::move(render_pass),
      [device](VkRenderPass&& x) { vkDestroyRenderPass(device, x, nullptr); });
}

ErrorOr<WithDeleter<VkPipeline>> MakePipeline(
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
  rs.cullMode = VK_CULL_MODE_BACK_BIT;
  rs.frontFace = VK_FRONT_FACE_CLOCKWISE;
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
  ds.depthCompareOp = VK_COMPARE_OP_LESS;
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
    return Error{fmt::format("Failed to create graphics pipeline: {}", err)};
  }

  return WithDeleter<VkPipeline>(std::move(pipeline), [device](VkPipeline&& x) {
    vkDestroyPipeline(device, x, nullptr);
  });
}

ErrorOr<WithDeleter<VkSemaphore>> MakeSemaphore(VkDevice device) {
  VkSemaphoreCreateInfo semaphore_create_info{};
  semaphore_create_info.sType = VK_STRUCTURE_TYPE_SEMAPHORE_CREATE_INFO;

  VkSemaphore semaphore;
  VkResult err =
      vkCreateSemaphore(device, &semaphore_create_info, nullptr, &semaphore);
  if (err != VK_SUCCESS) {
    return Error{fmt::format("Failed to create Vulkan semaphore: {}", err)};
  }

  return WithDeleter<VkSemaphore>(
      std::move(semaphore),
      [device](VkSemaphore&& x) { vkDestroySemaphore(device, x, nullptr); });
}

}  // namespace vulkan_utils