#include "vulkan_utils/vulkan_utils.h"

#include <fmt/format.h>
#include <vulkan/vulkan.h>

#include <array>
#include <iostream>
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
  VkDescriptorPoolCreateInfo descriptor_pool_create_info{};
  descriptor_pool_create_info.sType =
      VK_STRUCTURE_TYPE_DESCRIPTOR_POOL_CREATE_INFO;
  descriptor_pool_create_info.maxSets = max_sets;
  descriptor_pool_create_info.poolSizeCount = descriptor_pool_sizes.size();
  descriptor_pool_create_info.pPoolSizes = descriptor_pool_sizes.data();
  descriptor_pool_create_info.flags =
      VK_DESCRIPTOR_POOL_CREATE_FREE_DESCRIPTOR_SET_BIT;

  VkDescriptorPool pool;
  auto err = vkCreateDescriptorPool(device, &descriptor_pool_create_info,
                                    nullptr, &pool);
  if (err != VK_SUCCESS) {
    return Error{fmt::format("Failed to create descriptor pool: {}", err)};
  }
  return WithDeleter<VkDescriptorPool>(
      std::move(pool), [device](VkDescriptorPool&& x) {
        vkDestroyDescriptorPool(device, x, nullptr);
      });
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
  }
  return WithDeleter<VkDescriptorSetLayout>(
      std::move(layout), [device](VkDescriptorSetLayout&& x) {
        vkDestroyDescriptorSetLayout(device, x, nullptr);
      });
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

void SetImageLayout(VkCommandBuffer command_buffer, VkImage image,
                    VkImageAspectFlags image_aspect_flags,
                    VkImageLayout old_layout, VkImageLayout new_layout) {
  // Thanks to
  // https://harrylovescode.gitbooks.io/vulkan-api/content/chap07/chap07.html
  // for most of the logic in this function.
  VkImageMemoryBarrier image_memory_barrier = {};
  image_memory_barrier.sType = VK_STRUCTURE_TYPE_IMAGE_MEMORY_BARRIER;
  image_memory_barrier.pNext = nullptr;
  image_memory_barrier.oldLayout = old_layout;
  image_memory_barrier.newLayout = new_layout;
  image_memory_barrier.image = image;
  image_memory_barrier.subresourceRange.aspectMask = image_aspect_flags;
  image_memory_barrier.subresourceRange.baseMipLevel = 0;
  image_memory_barrier.subresourceRange.levelCount = 1;
  image_memory_barrier.subresourceRange.layerCount = 1;

  switch (old_layout) {
    case VK_IMAGE_LAYOUT_UNDEFINED:
      // Leave the bitfield empty here.
      break;
    case VK_IMAGE_LAYOUT_PREINITIALIZED:
      image_memory_barrier.srcAccessMask =
          VK_ACCESS_HOST_WRITE_BIT | VK_ACCESS_TRANSFER_WRITE_BIT;
      break;
    case VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL:
      image_memory_barrier.srcAccessMask = VK_ACCESS_COLOR_ATTACHMENT_WRITE_BIT;
      break;
    case VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL:
      image_memory_barrier.srcAccessMask =
          VK_ACCESS_DEPTH_STENCIL_ATTACHMENT_WRITE_BIT;
      break;
    case VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL:
      image_memory_barrier.srcAccessMask = VK_ACCESS_TRANSFER_READ_BIT;
      break;
    case VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL:
      image_memory_barrier.srcAccessMask = VK_ACCESS_SHADER_READ_BIT;
      break;
    default:
      // Not supported.
      assert(false);
  }

  switch (new_layout) {
    case VK_IMAGE_LAYOUT_PRESENT_SRC_KHR:
      // Intentionally left empty (mainly because I don't know what to do here).
      break;
    case VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL:
      image_memory_barrier.dstAccessMask = VK_ACCESS_TRANSFER_WRITE_BIT;
      break;
    case VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL:
      image_memory_barrier.srcAccessMask |= VK_ACCESS_TRANSFER_READ_BIT;
      image_memory_barrier.dstAccessMask = VK_ACCESS_TRANSFER_READ_BIT;
      break;
    case VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL:
      image_memory_barrier.dstAccessMask = VK_ACCESS_COLOR_ATTACHMENT_WRITE_BIT;
      image_memory_barrier.srcAccessMask = VK_ACCESS_TRANSFER_READ_BIT;
      break;
    case VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL:
      image_memory_barrier.dstAccessMask |=
          VK_ACCESS_DEPTH_STENCIL_ATTACHMENT_WRITE_BIT;
      break;
    case VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL:
      image_memory_barrier.srcAccessMask =
          VK_ACCESS_HOST_WRITE_BIT | VK_ACCESS_TRANSFER_WRITE_BIT;
      image_memory_barrier.dstAccessMask = VK_ACCESS_SHADER_READ_BIT;
      break;
    default:
      // Not supported.
      assert(false);
  }

  if (old_layout == VK_IMAGE_LAYOUT_UNDEFINED) {
    image_memory_barrier.srcAccessMask = 0;
  }

  VkPipelineStageFlagBits src_pipeline_stage_flags =
      VK_PIPELINE_STAGE_ALL_GRAPHICS_BIT;
  VkPipelineStageFlagBits dst_pipeline_stage_flags =
      VK_PIPELINE_STAGE_ALL_GRAPHICS_BIT;
  vkCmdPipelineBarrier(command_buffer, src_pipeline_stage_flags,
                       dst_pipeline_stage_flags, 0, 0, nullptr, 0, nullptr, 1,
                       &image_memory_barrier);
}

ErrorOr<WithDeleter<VkImageView>> MakeImageView(
    VkDevice device, VkImage image, VkFormat format,
    VkImageAspectFlags aspect_mask) {
  VkImageViewCreateInfo view_info{};
  view_info.sType = VK_STRUCTURE_TYPE_IMAGE_VIEW_CREATE_INFO;
  view_info.image = image;
  view_info.viewType = VK_IMAGE_VIEW_TYPE_2D;
  view_info.format = format;
  view_info.subresourceRange.aspectMask = aspect_mask;
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
    VkShaderModule fragment_shader_module, const MakePipelineOptions& options) {
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
  ia.topology = options.primitive_topology;
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
  rs.lineWidth = options.line_width;
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
  ds.depthTestEnable = (options.depth_test ? VK_TRUE : VK_FALSE);
  ds.depthWriteEnable = (options.depth_write ? VK_TRUE : VK_FALSE);
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

namespace {

std::vector<std::string> GetSupportedValidationLayers() {
#ifdef NDEBUG
  constexpr bool kEnableValidationLayers = false;
#else
  constexpr bool kEnableValidationLayers = true;
#endif
  if (!kEnableValidationLayers) {
    return std::vector<std::string>();
  }

  const std::vector<std::string> PREFERRED_VALIDATION_LAYERS = {
      "VK_LAYER_KHRONOS_validation"};

  uint32_t layer_count;
  vkEnumerateInstanceLayerProperties(&layer_count, nullptr);

  std::vector<VkLayerProperties> available_layers(layer_count);
  vkEnumerateInstanceLayerProperties(&layer_count, available_layers.data());

  std::vector<std::string> results;
  results.reserve(PREFERRED_VALIDATION_LAYERS.size());
  for (const auto& layer_name : PREFERRED_VALIDATION_LAYERS) {
    for (const auto& layer_properties : available_layers) {
      if (layer_name == layer_properties.layerName) {
        results.push_back(layer_properties.layerName);
        break;
      }
    }
  }

  return results;
}

}  // namespace

ErrorOr<WithDeleter<VkDebugUtilsMessengerEXT>> MakeDebugUtilsMessenger(
    VkInstance instance, VkDebugUtilsMessageSeverityFlagBitsEXT filter_severity,
    VkDebugUtilsMessageTypeFlagsEXT filter_message_type,
    PFN_vkDebugUtilsMessengerCallbackEXT user_callback) {
  VkDebugUtilsMessengerCreateInfoEXT create_info{};
  create_info.sType = VK_STRUCTURE_TYPE_DEBUG_UTILS_MESSENGER_CREATE_INFO_EXT;
  create_info.messageSeverity = filter_severity;
  create_info.messageType = filter_message_type;
  create_info.pfnUserCallback = user_callback;
  create_info.pUserData = nullptr;  // Optional

  auto CreateDebugUtilsMessengerEXT =
      reinterpret_cast<PFN_vkCreateDebugUtilsMessengerEXT>(
          vkGetInstanceProcAddr(instance, "vkCreateDebugUtilsMessengerEXT"));
  if (!CreateDebugUtilsMessengerEXT) {
    return Error{"Could not find function `vkCreateDebugUtilsMessengerEXT`."};
  }
  auto DestroyDebugUtilsMessengerEXT =
      reinterpret_cast<PFN_vkDestroyDebugUtilsMessengerEXT>(
          vkGetInstanceProcAddr(instance, "vkDestroyDebugUtilsMessengerEXT"));
  if (!DestroyDebugUtilsMessengerEXT) {
    return Error{"Could not find function `vkDestroyDebugUtilsMessengerEXT`."};
  }

  VkDebugUtilsMessengerEXT debug_utils_messenger;
  VkResult err = CreateDebugUtilsMessengerEXT(instance, &create_info, nullptr,
                                              &debug_utils_messenger);
  if (err != VK_SUCCESS) {
    return Error{fmt::format("Error creating debug utils messenger: {}", err),
                 err};
  }

  return WithDeleter<VkDebugUtilsMessengerEXT>(
      std::move(debug_utils_messenger),
      [instance, DestroyDebugUtilsMessengerEXT](VkDebugUtilsMessengerEXT&& x) {
        DestroyDebugUtilsMessengerEXT(instance, x, nullptr);
      });
}

ErrorOr<WithDeleter<VkInstance>> MakeInstance(
    const VkApplicationInfo& application_info,
    const std::vector<const char*>& instance_extensions) {
  VkInstanceCreateInfo instance_create_info{};
  instance_create_info.sType = VK_STRUCTURE_TYPE_INSTANCE_CREATE_INFO;
  instance_create_info.pApplicationInfo =
      const_cast<VkApplicationInfo*>(&application_info);

  std::vector<std::string> validation_layers = GetSupportedValidationLayers();
  std::vector<const char*> validation_layers_c_str;
  validation_layers_c_str.reserve(validation_layers.size());
  for (const auto& str : validation_layers) {
    std::cerr << "Enabling Vulkan validation layer: " << str << std::endl;
    validation_layers_c_str.push_back(str.c_str());
  }
  instance_create_info.enabledLayerCount =
      static_cast<uint32_t>(validation_layers_c_str.size());
  instance_create_info.ppEnabledLayerNames = validation_layers_c_str.data();

  std::vector<const char*> extended_instance_extensions(instance_extensions);
  if (!validation_layers.empty()) {
    extended_instance_extensions.push_back(VK_EXT_DEBUG_UTILS_EXTENSION_NAME);
  }

  instance_create_info.enabledExtensionCount =
      extended_instance_extensions.size();
  instance_create_info.ppEnabledExtensionNames =
      const_cast<const char**>(extended_instance_extensions.data());

  VkInstance instance;
  VkResult err = vkCreateInstance(&instance_create_info, nullptr, &instance);
  if (err != VK_SUCCESS) {
    return Error{fmt::format("Failed to create Vulkan instance: {}", err)};
  }

  return WithDeleter<VkInstance>(std::move(instance), [](VkInstance&& x) {
    vkDestroyInstance(x, nullptr);
  });
}

ErrorOr<std::vector<VkPhysicalDevice>> EnumeratePhysicalDevices(
    VkInstance instance) {
  VkResult err;

  uint32_t device_count = 0;
  err = vkEnumeratePhysicalDevices(instance, &device_count, nullptr);
  if (err != VK_SUCCESS) {
    return Error{fmt::format("Failed to enumerate physical devices: {}", err)};
  }
  if (device_count == 0) {
    return Error{"No Vulkan-compatible physical devices found."};
  }

  std::vector<VkPhysicalDevice> physical_devices(device_count);
  err = vkEnumeratePhysicalDevices(instance, &device_count,
                                   physical_devices.data());
  if (err != VK_SUCCESS) {
    return Error{
        fmt::format("Failed to re-enumerate physical devices: {}", err)};
  }

  return physical_devices;
}

namespace {
std::vector<VkQueueFamilyProperties> GetQueueFamilies(
    VkPhysicalDevice physical_device) {
  uint32_t queue_family_count = 0;
  vkGetPhysicalDeviceQueueFamilyProperties(physical_device, &queue_family_count,
                                           nullptr);

  std::vector<VkQueueFamilyProperties> queue_families(queue_family_count);
  vkGetPhysicalDeviceQueueFamilyProperties(physical_device, &queue_family_count,
                                           queue_families.data());
  return queue_families;
}

}  // namespace

std::optional<uint32_t> FindFirstQueueFamilyIndex(
    VkPhysicalDevice physical_device, VkQueueFlagBits queue_family_bits) {
  std::vector<VkQueueFamilyProperties> queue_families =
      GetQueueFamilies(physical_device);

  for (size_t i = 0; i < queue_families.size(); ++i) {
    if (queue_families[i].queueFlags & queue_family_bits) {
      return static_cast<uint32_t>(i);
    }
  }

  return std::nullopt;
}

ErrorOr<WithDeleter<VkDevice>> MakeDevice(
    VkPhysicalDevice physical_device, uint32_t device_queue,
    const std::vector<const char*>& device_extensions) {
  VkDeviceQueueCreateInfo queue_create_info{};
  queue_create_info.sType = VK_STRUCTURE_TYPE_DEVICE_QUEUE_CREATE_INFO;
  queue_create_info.queueFamilyIndex = device_queue;
  queue_create_info.queueCount = 1;
  float queue_priority = 1.0f;
  queue_create_info.pQueuePriorities = &queue_priority;

  VkPhysicalDeviceFeatures device_features{};
  VkDeviceCreateInfo device_create_info{};
  device_create_info.sType = VK_STRUCTURE_TYPE_DEVICE_CREATE_INFO;
  device_create_info.pQueueCreateInfos = &queue_create_info;
  device_create_info.queueCreateInfoCount = 1;
  device_create_info.pEnabledFeatures = &device_features;
  device_create_info.enabledExtensionCount =
      static_cast<uint32_t>(device_extensions.size());
  device_create_info.ppEnabledExtensionNames = device_extensions.data();
  device_create_info.enabledLayerCount = 0;

  VkDevice device;
  VkResult err =
      vkCreateDevice(physical_device, &device_create_info, nullptr, &device);
  if (err != VK_SUCCESS) {
    return Error{fmt::format("Failed to create logical device: {}", err)};
  }

  return WithDeleter<VkDevice>(
      std::move(device), [](VkDevice&& x) { vkDestroyDevice(x, nullptr); });
}

ErrorOr<WithDeleter<VkSwapchainKHR>> MakeSwapChain(
    VkDevice device, VkSurfaceKHR surface, uint32_t min_image_count,
    VkSurfaceFormatKHR surface_format, VkPresentModeKHR present_mode,
    VkExtent2D swap_chain_extent, VkSurfaceTransformFlagBitsKHR pre_transform,
    uint32_t graphics_queue_family_index, uint32_t present_queue_family_index) {
  VkSwapchainCreateInfoKHR swap_chain_create_info{};
  swap_chain_create_info.sType = VK_STRUCTURE_TYPE_SWAPCHAIN_CREATE_INFO_KHR;
  swap_chain_create_info.surface = surface;
  swap_chain_create_info.minImageCount = min_image_count;
  swap_chain_create_info.imageFormat = surface_format.format;
  swap_chain_create_info.imageColorSpace = surface_format.colorSpace;
  swap_chain_create_info.imageExtent = swap_chain_extent;
  swap_chain_create_info.imageArrayLayers = 1;
  swap_chain_create_info.imageUsage = VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT;
  if (graphics_queue_family_index != present_queue_family_index) {
    // According to
    // https://vulkan-tutorial.com/Drawing_a_triangle/Presentation/Window_surface,
    // this isn't the most efficient setup in the case that the graphics queue
    // is separate from the present queue, however it's also apparently an
    // uncommon situation.
    swap_chain_create_info.imageSharingMode = VK_SHARING_MODE_CONCURRENT;
    std::vector<uint32_t> queue_family_indices = {graphics_queue_family_index,
                                                  present_queue_family_index};
    swap_chain_create_info.queueFamilyIndexCount = queue_family_indices.size();
    swap_chain_create_info.pQueueFamilyIndices = queue_family_indices.data();
  } else {
    swap_chain_create_info.imageSharingMode = VK_SHARING_MODE_EXCLUSIVE;
    swap_chain_create_info.queueFamilyIndexCount = 0;      // Optional
    swap_chain_create_info.pQueueFamilyIndices = nullptr;  // Optional
  }
  swap_chain_create_info.preTransform = pre_transform;
  swap_chain_create_info.compositeAlpha = VK_COMPOSITE_ALPHA_OPAQUE_BIT_KHR;
  swap_chain_create_info.presentMode = present_mode;
  swap_chain_create_info.clipped = VK_TRUE;
  swap_chain_create_info.oldSwapchain =
      VK_NULL_HANDLE;  // TODO: Handle resetting swap chains.

  VkSwapchainKHR swap_chain;
  VkResult err = vkCreateSwapchainKHR(device, &swap_chain_create_info, nullptr,
                                      &swap_chain);
  if (err != VK_SUCCESS) {
    return Error{fmt::format("Error creating Vulkan swap chain: {}", err)};
  }

  return WithDeleter<VkSwapchainKHR>(
      std::move(swap_chain), [device](VkSwapchainKHR&& x) {
        vkDestroySwapchainKHR(device, x, nullptr);
      });
}

ErrorOr<WithDeleter<VkFramebuffer>> MakeFramebuffer(
    VkDevice device, VkRenderPass render_pass,
    const std::vector<VkImageView>& color_attachments,
    const std::optional<VkImageView>& maybe_depth_stencil_attachment,
    VkExtent2D extent) {
  std::vector<VkImageView> attachments;
  attachments.reserve(color_attachments.size() +
                      (maybe_depth_stencil_attachment ? 1 : 0));
  for (const auto& color_attachment : color_attachments) {
    attachments.push_back(color_attachment);
  }
  if (maybe_depth_stencil_attachment) {
    attachments.push_back(*maybe_depth_stencil_attachment);
  }

  VkFramebufferCreateInfo framebuffer_info{};
  framebuffer_info.sType = VK_STRUCTURE_TYPE_FRAMEBUFFER_CREATE_INFO;
  framebuffer_info.renderPass = render_pass;
  framebuffer_info.attachmentCount = static_cast<uint32_t>(attachments.size());
  framebuffer_info.pAttachments = attachments.data();
  framebuffer_info.width = extent.width;
  framebuffer_info.height = extent.height;
  framebuffer_info.layers = 1;

  VkFramebuffer framebuffer;
  VkResult err =
      vkCreateFramebuffer(device, &framebuffer_info, nullptr, &framebuffer);
  if (err != VK_SUCCESS) {
    return Error{fmt::format("Failed to create Vulkan framebuffer: {}", err)};
  }

  return WithDeleter<VkFramebuffer>(std::move(framebuffer),
                                    [device](VkFramebuffer&& x) {
                                      vkDestroyFramebuffer(device, x, nullptr);
                                    });
}

ErrorOr<WithDeleter<VkCommandPool>> MakeCommandPool(
    VkDevice device, uint32_t queue_family_index) {
  VkCommandPoolCreateInfo command_pool_info{};
  command_pool_info.sType = VK_STRUCTURE_TYPE_COMMAND_POOL_CREATE_INFO;
  command_pool_info.queueFamilyIndex = queue_family_index;
  command_pool_info.flags = VK_COMMAND_POOL_CREATE_TRANSIENT_BIT |
                            VK_COMMAND_POOL_CREATE_RESET_COMMAND_BUFFER_BIT;

  VkCommandPool command_pool;
  VkResult err =
      vkCreateCommandPool(device, &command_pool_info, nullptr, &command_pool);
  if (err != VK_SUCCESS) {
    return Error{fmt::format("Failed to create command pool: {}", err)};
  }

  return WithDeleter<VkCommandPool>(std::move(command_pool),
                                    [device](VkCommandPool&& x) {
                                      vkDestroyCommandPool(device, x, nullptr);
                                    });
}

ErrorOr<std::vector<VkCommandBuffer>> MakeCommandBuffers(
    VkDevice device, VkCommandPool command_pool, uint32_t num_command_buffers) {
  std::vector<VkCommandBuffer> command_buffers;
  command_buffers.resize(num_command_buffers);
  VkCommandBufferAllocateInfo command_buffer_alloc_info{};
  command_buffer_alloc_info.sType =
      VK_STRUCTURE_TYPE_COMMAND_BUFFER_ALLOCATE_INFO;
  command_buffer_alloc_info.commandPool = command_pool;
  command_buffer_alloc_info.level = VK_COMMAND_BUFFER_LEVEL_PRIMARY;
  command_buffer_alloc_info.commandBufferCount = num_command_buffers;

  VkResult err = vkAllocateCommandBuffers(device, &command_buffer_alloc_info,
                                          command_buffers.data());
  if (err != VK_SUCCESS) {
    return Error{fmt::format("Failed to allocate command buffers: {}", err)};
  }

  return command_buffers;
}

}  // namespace vulkan_utils