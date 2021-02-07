#include "renderer.h"

#include <fmt/format.h>

#include <iostream>

namespace {

#include "src_gen/color_frag.h"
#include "src_gen/color_vert.h"

// Note that the vertex data and the projection matrix assume OpenGL. With
// Vulkan Y is negated in clip space and the near/far plane is at 0/1 instead
// of -1/1. These will be corrected for by an extra transformation when
// calculating the modelview-projection matrix.
const float VERTEX_DATA[] = {  // Y up, front = CCW
    0.0f, 0.5f, 1.0f, 0.0f,  0.0f, -0.5f, -0.5f, 0.0f,
    1.0f, 0.0f, 0.5f, -0.5f, 0.0f, 0.0f,  1.0f};

const int UNIFORM_DATA_SIZE = 16 * sizeof(float);

inline VkDeviceSize aligned(VkDeviceSize v, VkDeviceSize byteAlign) {
  return (v + byteAlign - 1) & ~(byteAlign - 1);
}

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

Renderer::ErrorOr<VkShaderModule> CreateShader(VkDevice device,
                                               const char* data, size_t size) {
  VkShaderModuleCreateInfo shaderInfo;
  memset(&shaderInfo, 0, sizeof(shaderInfo));
  shaderInfo.sType = VK_STRUCTURE_TYPE_SHADER_MODULE_CREATE_INFO;
  shaderInfo.codeSize = size;
  shaderInfo.pCode = reinterpret_cast<const uint32_t*>(data);
  VkShaderModule shaderModule;
  VkResult err =
      vkCreateShaderModule(device, &shaderInfo, nullptr, &shaderModule);
  if (err != VK_SUCCESS) {
    return Renderer::Error{
        fmt::format("Failed to create shader module: {}", err)};
  }

  return shaderModule;
}
}  // namespace

Renderer::MemoryAllocator::Allocation::~Allocation() {
  vkFreeMemory(device_, memory_, nullptr);
}

Renderer::MemoryAllocator::MemoryAllocator(VkPhysicalDevice physical_device,
                                           VkDevice device)
    : physical_device_(physical_device), device_(device) {}

auto Renderer::MemoryAllocator::Allocate(
    VkBuffer buffer, VkMemoryPropertyFlags memory_property_flags)
    -> ErrorOr<std::unique_ptr<Allocation>> {
  VkMemoryRequirements mem_reqs;
  vkGetBufferMemoryRequirements(device_, buffer, &mem_reqs);

  return Allocate(mem_reqs.size, mem_reqs.memoryTypeBits,
                  memory_property_flags);
}

auto Renderer::MemoryAllocator::Allocate(
    VkDeviceSize size, uint32_t type_filter,
    VkMemoryPropertyFlags memory_property_flags)
    -> ErrorOr<std::unique_ptr<Allocation>> {
  auto error_or_memory_type =
      FindMemoryTypeIndex(physical_device_, type_filter, memory_property_flags);

  if (auto error = std::get_if<Error>(&error_or_memory_type)) {
    return *error;
  }

  return Allocate(size, std::get<uint32_t>(error_or_memory_type));
}

auto Renderer::MemoryAllocator::Allocate(VkDeviceSize size,
                                         uint32_t memory_type)
    -> ErrorOr<std::unique_ptr<Allocation>> {
  VkMemoryAllocateInfo mem_alloc_info = {VK_STRUCTURE_TYPE_MEMORY_ALLOCATE_INFO,
                                         nullptr, size, memory_type};

  VkDeviceMemory vk_memory;
  auto err = vkAllocateMemory(device_, &mem_alloc_info, nullptr, &vk_memory);
  if (err != VK_SUCCESS) {
    return Error{
        fmt::format("Failed to allocate {} bytes of memory: {}", size, err)};
  }
  return std::make_unique<Allocation>(device_, vk_memory);
}

// static
auto Renderer::Create(VkInstance instance, VkPhysicalDevice physical_device,
                      VkDevice device) -> ErrorOr<Renderer> {
  RendererConstructorData data;
  data.instance = instance;
  data.physical_device = physical_device;
  data.device = device;

  data.allocator.emplace(data.physical_device, data.device);

  VkBufferCreateInfo vertex_buffer_create_info;
  memset(&vertex_buffer_create_info, 0, sizeof(vertex_buffer_create_info));
  vertex_buffer_create_info.sType = VK_STRUCTURE_TYPE_BUFFER_CREATE_INFO;
  vertex_buffer_create_info.size = sizeof(VERTEX_DATA);
  vertex_buffer_create_info.usage = VK_BUFFER_USAGE_VERTEX_BUFFER_BIT;

  VkResult err = vkCreateBuffer(data.device, &vertex_buffer_create_info,
                                nullptr, &data.vertex_buffer);
  if (err != VK_SUCCESS) {
    return Error{fmt::format("Failed to create buffer: {}", err)};
  }

  auto allocate_result = data.allocator->Allocate(
      data.vertex_buffer, VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT |
                              VK_MEMORY_PROPERTY_HOST_COHERENT_BIT);
  if (auto error = std::get_if<Error>(&allocate_result)) {
    return *error;
  }
  data.vertex_buffer_mem = std::move(std::get<1>(allocate_result));

  err = vkBindBufferMemory(data.device, data.vertex_buffer,
                           data.vertex_buffer_mem->memory(), 0);
  if (err != VK_SUCCESS) {
    return Error{fmt::format("Failed to bind buffer memory: {}", err)};
  }

  uint8_t* p;
  err = vkMapMemory(data.device, data.vertex_buffer_mem->memory(), 0,
                    vertex_buffer_create_info.size, 0,
                    reinterpret_cast<void**>(&p));
  if (err != VK_SUCCESS) {
    return Error{fmt::format("Failed to map memory: {}", err)};
  }

  memcpy(p, VERTEX_DATA, sizeof(VERTEX_DATA));

  vkUnmapMemory(data.device, data.vertex_buffer_mem->memory());

  VkVertexInputBindingDescription vertex_binding_desc = {
      0,  // binding
      5 * sizeof(float), VK_VERTEX_INPUT_RATE_VERTEX};
  VkVertexInputAttributeDescription vertex_attr_desc[] = {
      {    // position
       0,  // location
       0,  // binding
       VK_FORMAT_R32G32_SFLOAT, 0},
      {// color
       1, 0, VK_FORMAT_R32G32B32_SFLOAT, 2 * sizeof(float)}};

  VkPipelineVertexInputStateCreateInfo vertex_input_info;
  vertex_input_info.sType =
      VK_STRUCTURE_TYPE_PIPELINE_VERTEX_INPUT_STATE_CREATE_INFO;
  vertex_input_info.pNext = nullptr;
  vertex_input_info.flags = 0;
  vertex_input_info.vertexBindingDescriptionCount = 1;
  vertex_input_info.pVertexBindingDescriptions = &vertex_binding_desc;
  vertex_input_info.vertexAttributeDescriptionCount = 2;
  vertex_input_info.pVertexAttributeDescriptions = vertex_attr_desc;

  // Set up descriptor set and its layout.
  VkDescriptorPoolSize descPoolSizes = {VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER,
                                        uint32_t(concurrentFrameCount)};
  VkDescriptorPoolCreateInfo descPoolInfo;
  memset(&descPoolInfo, 0, sizeof(descPoolInfo));
  descPoolInfo.sType = VK_STRUCTURE_TYPE_DESCRIPTOR_POOL_CREATE_INFO;
  descPoolInfo.maxSets = concurrentFrameCount;
  descPoolInfo.poolSizeCount = 1;
  descPoolInfo.pPoolSizes = &descPoolSizes;
  err = m_devFuncs->vkCreateDescriptorPool(dev, &descPoolInfo, nullptr,
                                           &m_descPool);
  if (err != VK_SUCCESS) qFatal("Failed to create descriptor pool: %d", err);

  VkDescriptorSetLayoutBinding layoutBinding = {
      0,  // binding
      VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER, 1, VK_SHADER_STAGE_VERTEX_BIT,
      nullptr};
  VkDescriptorSetLayoutCreateInfo descLayoutInfo = {
      VK_STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_CREATE_INFO, nullptr, 0, 1,
      &layoutBinding};
  err = m_devFuncs->vkCreateDescriptorSetLayout(dev, &descLayoutInfo, nullptr,
                                                &m_descSetLayout);
  if (err != VK_SUCCESS)
    qFatal("Failed to create descriptor set layout: %d", err);

  for (int i = 0; i < concurrentFrameCount; ++i) {
    VkDescriptorSetAllocateInfo descSetAllocInfo = {
        VK_STRUCTURE_TYPE_DESCRIPTOR_SET_ALLOCATE_INFO, nullptr, m_descPool, 1,
        &m_descSetLayout};
    err = m_devFuncs->vkAllocateDescriptorSets(dev, &descSetAllocInfo,
                                               &m_descSet[i]);
    if (err != VK_SUCCESS) qFatal("Failed to allocate descriptor set: %d", err);

    VkWriteDescriptorSet descWrite;
    memset(&descWrite, 0, sizeof(descWrite));
    descWrite.sType = VK_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET;
    descWrite.dstSet = m_descSet[i];
    descWrite.descriptorCount = 1;
    descWrite.descriptorType = VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER;
    descWrite.pBufferInfo = &m_uniformBufInfo[i];
    m_devFuncs->vkUpdateDescriptorSets(dev, 1, &descWrite, 0, nullptr);
  }

  // Pipeline cache
  VkPipelineCacheCreateInfo pipelineCacheInfo;
  memset(&pipelineCacheInfo, 0, sizeof(pipelineCacheInfo));
  pipelineCacheInfo.sType = VK_STRUCTURE_TYPE_PIPELINE_CACHE_CREATE_INFO;
  err = m_devFuncs->vkCreatePipelineCache(dev, &pipelineCacheInfo, nullptr,
                                          &m_pipelineCache);
  if (err != VK_SUCCESS) qFatal("Failed to create pipeline cache: %d", err);

  // Pipeline layout
  VkPipelineLayoutCreateInfo pipelineLayoutInfo;
  memset(&pipelineLayoutInfo, 0, sizeof(pipelineLayoutInfo));
  pipelineLayoutInfo.sType = VK_STRUCTURE_TYPE_PIPELINE_LAYOUT_CREATE_INFO;
  pipelineLayoutInfo.setLayoutCount = 1;
  pipelineLayoutInfo.pSetLayouts = &m_descSetLayout;
  err = m_devFuncs->vkCreatePipelineLayout(dev, &pipelineLayoutInfo, nullptr,
                                           &m_pipelineLayout);
  if (err != VK_SUCCESS) qFatal("Failed to create pipeline layout: %d", err);

  // Shaders
  VkShaderModule vertShaderModule = createShader(
      device_, reinterpret_cast<const char*>(color_vert, color_vert_len));
  VkShaderModule fragShaderModule = createShader(
      device_, reinterpret_cast<const char*>(color_frag, color_frag_len));

  // Graphics pipeline
  VkGraphicsPipelineCreateInfo pipelineInfo;
  memset(&pipelineInfo, 0, sizeof(pipelineInfo));
  pipelineInfo.sType = VK_STRUCTURE_TYPE_GRAPHICS_PIPELINE_CREATE_INFO;

  VkPipelineShaderStageCreateInfo shaderStages[2] = {
      {VK_STRUCTURE_TYPE_PIPELINE_SHADER_STAGE_CREATE_INFO, nullptr, 0,
       VK_SHADER_STAGE_VERTEX_BIT, vertShaderModule, "main", nullptr},
      {VK_STRUCTURE_TYPE_PIPELINE_SHADER_STAGE_CREATE_INFO, nullptr, 0,
       VK_SHADER_STAGE_FRAGMENT_BIT, fragShaderModule, "main", nullptr}};
  pipelineInfo.stageCount = 2;
  pipelineInfo.pStages = shaderStages;

  pipelineInfo.pVertexInputState = &vertexInputInfo;

  VkPipelineInputAssemblyStateCreateInfo ia;
  memset(&ia, 0, sizeof(ia));
  ia.sType = VK_STRUCTURE_TYPE_PIPELINE_INPUT_ASSEMBLY_STATE_CREATE_INFO;
  ia.topology = VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST;
  pipelineInfo.pInputAssemblyState = &ia;

  // The viewport and scissor will be set dynamically via
  // vkCmdSetViewport/Scissor. This way the pipeline does not need to be
  // touched when resizing the window.
  VkPipelineViewportStateCreateInfo vp;
  memset(&vp, 0, sizeof(vp));
  vp.sType = VK_STRUCTURE_TYPE_PIPELINE_VIEWPORT_STATE_CREATE_INFO;
  vp.viewportCount = 1;
  vp.scissorCount = 1;
  pipelineInfo.pViewportState = &vp;

  VkPipelineRasterizationStateCreateInfo rs;
  memset(&rs, 0, sizeof(rs));
  rs.sType = VK_STRUCTURE_TYPE_PIPELINE_RASTERIZATION_STATE_CREATE_INFO;
  rs.polygonMode = VK_POLYGON_MODE_FILL;
  rs.cullMode = VK_CULL_MODE_NONE;  // we want the back face as well
  rs.frontFace = VK_FRONT_FACE_COUNTER_CLOCKWISE;
  rs.lineWidth = 1.0f;
  pipelineInfo.pRasterizationState = &rs;

  VkPipelineMultisampleStateCreateInfo ms;
  memset(&ms, 0, sizeof(ms));
  ms.sType = VK_STRUCTURE_TYPE_PIPELINE_MULTISAMPLE_STATE_CREATE_INFO;
  // Enable multisampling.
  ms.rasterizationSamples = m_window->sampleCountFlagBits();
  pipelineInfo.pMultisampleState = &ms;

  VkPipelineDepthStencilStateCreateInfo ds;
  memset(&ds, 0, sizeof(ds));
  ds.sType = VK_STRUCTURE_TYPE_PIPELINE_DEPTH_STENCIL_STATE_CREATE_INFO;
  ds.depthTestEnable = VK_TRUE;
  ds.depthWriteEnable = VK_TRUE;
  ds.depthCompareOp = VK_COMPARE_OP_LESS_OR_EQUAL;
  pipelineInfo.pDepthStencilState = &ds;

  VkPipelineColorBlendStateCreateInfo cb;
  memset(&cb, 0, sizeof(cb));
  cb.sType = VK_STRUCTURE_TYPE_PIPELINE_COLOR_BLEND_STATE_CREATE_INFO;
  // no blend, write out all of rgba
  VkPipelineColorBlendAttachmentState att;
  memset(&att, 0, sizeof(att));
  att.colorWriteMask = 0xF;
  cb.attachmentCount = 1;
  cb.pAttachments = &att;
  pipelineInfo.pColorBlendState = &cb;

  VkDynamicState dynEnable[] = {VK_DYNAMIC_STATE_VIEWPORT,
                                VK_DYNAMIC_STATE_SCISSOR};
  VkPipelineDynamicStateCreateInfo dyn;
  memset(&dyn, 0, sizeof(dyn));
  dyn.sType = VK_STRUCTURE_TYPE_PIPELINE_DYNAMIC_STATE_CREATE_INFO;
  dyn.dynamicStateCount = sizeof(dynEnable) / sizeof(VkDynamicState);
  dyn.pDynamicStates = dynEnable;
  pipelineInfo.pDynamicState = &dyn;

  pipelineInfo.layout = m_pipelineLayout;
  pipelineInfo.renderPass = m_window->defaultRenderPass();

  err = m_devFuncs->vkCreateGraphicsPipelines(
      dev, m_pipelineCache, 1, &pipelineInfo, nullptr, &m_pipeline);
  if (err != VK_SUCCESS) qFatal("Failed to create graphics pipeline: %d", err);

  if (vertShaderModule)
    m_devFuncs->vkDestroyShaderModule(dev, vertShaderModule, nullptr);
  if (fragShaderModule)
    m_devFuncs->vkDestroyShaderModule(dev, fragShaderModule, nullptr);

  // Projection matrix
  m_proj = m_window->clipCorrectionMatrix();  // adjust for Vulkan-OpenGL clip
                                              // space differences
  const QSize sz = m_window->swapChainImageSize();
  m_proj.perspective(45.0f, sz.width() / (float)sz.height(), 0.01f, 100.0f);
  m_proj.translate(0, 0, -4);

  return Renderer(data);
}

Renderer::Renderer(RendererConstructorData data) : data_(std::move(data)) {}

Renderer::~Renderer() {
  if (m_pipeline) {
    m_devFuncs->vkDestroyPipeline(dev, m_pipeline, nullptr);
    m_pipeline = VK_NULL_HANDLE;
  }

  if (m_pipelineLayout) {
    m_devFuncs->vkDestroyPipelineLayout(dev, m_pipelineLayout, nullptr);
    m_pipelineLayout = VK_NULL_HANDLE;
  }

  if (m_pipelineCache) {
    m_devFuncs->vkDestroyPipelineCache(dev, m_pipelineCache, nullptr);
    m_pipelineCache = VK_NULL_HANDLE;
  }

  if (m_descSetLayout) {
    m_devFuncs->vkDestroyDescriptorSetLayout(dev, m_descSetLayout, nullptr);
    m_descSetLayout = VK_NULL_HANDLE;
  }

  if (m_descPool) {
    m_devFuncs->vkDestroyDescriptorPool(dev, m_descPool, nullptr);
    m_descPool = VK_NULL_HANDLE;
  }

  if (m_buf) {
    m_devFuncs->vkDestroyBuffer(dev, m_buf, nullptr);
    m_buf = VK_NULL_HANDLE;
  }

  if (m_bufMem) {
    m_devFuncs->vkFreeMemory(dev, m_bufMem, nullptr);
    m_bufMem = VK_NULL_HANDLE;
  }
}

std::function<void()> Renderer::RenderFrame(
    VkCommandBuffer frame_command_buffer,
    const std::pair<uint32_t, uint32_t>& output_surface_size) {
  VkDevice dev = m_window->device();
  VkCommandBuffer cb = m_window->currentCommandBuffer();
  const QSize sz = m_window->swapChainImageSize();

  VkClearColorValue clearColor = {{0, 0, 0, 1}};
  VkClearDepthStencilValue clearDS = {1, 0};
  VkClearValue clearValues[3];
  memset(clearValues, 0, sizeof(clearValues));
  clearValues[0].color = clearValues[2].color = clearColor;
  clearValues[1].depthStencil = clearDS;

  VkRenderPassBeginInfo rpBeginInfo;
  memset(&rpBeginInfo, 0, sizeof(rpBeginInfo));
  rpBeginInfo.sType = VK_STRUCTURE_TYPE_RENDER_PASS_BEGIN_INFO;
  rpBeginInfo.renderPass = m_window->defaultRenderPass();
  rpBeginInfo.framebuffer = m_window->currentFramebuffer();
  rpBeginInfo.renderArea.extent.width = sz.width();
  rpBeginInfo.renderArea.extent.height = sz.height();
  rpBeginInfo.clearValueCount =
      m_window->sampleCountFlagBits() > VK_SAMPLE_COUNT_1_BIT ? 3 : 2;
  rpBeginInfo.pClearValues = clearValues;
  VkCommandBuffer cmdBuf = m_window->currentCommandBuffer();
  m_devFuncs->vkCmdBeginRenderPass(cmdBuf, &rpBeginInfo,
                                   VK_SUBPASS_CONTENTS_INLINE);

  quint8* p;
  VkResult err = m_devFuncs->vkMapMemory(
      dev, m_bufMem, m_uniformBufInfo[m_window->currentFrame()].offset,
      UNIFORM_DATA_SIZE, 0, reinterpret_cast<void**>(&p));
  if (err != VK_SUCCESS) qFatal("Failed to map memory: %d", err);
  QMatrix4x4 m = m_proj;
  m.rotate(m_rotation, 0, 1, 0);
  memcpy(p, m.constData(), 16 * sizeof(float));
  m_devFuncs->vkUnmapMemory(dev, m_bufMem);

  // Not exactly a real animation system, just advance on every frame for now.
  m_rotation += 1.0f;

  m_devFuncs->vkCmdBindPipeline(cb, VK_PIPELINE_BIND_POINT_GRAPHICS,
                                m_pipeline);
  m_devFuncs->vkCmdBindDescriptorSets(
      cb, VK_PIPELINE_BIND_POINT_GRAPHICS, m_pipelineLayout, 0, 1,
      &m_descSet[m_window->currentFrame()], 0, nullptr);
  VkDeviceSize vbOffset = 0;
  m_devFuncs->vkCmdBindVertexBuffers(cb, 0, 1, &m_buf, &vbOffset);

  VkViewport viewport;
  viewport.x = viewport.y = 0;
  viewport.width = sz.width();
  viewport.height = sz.height();
  viewport.minDepth = 0;
  viewport.maxDepth = 1;
  m_devFuncs->vkCmdSetViewport(cb, 0, 1, &viewport);

  VkRect2D scissor;
  scissor.offset.x = scissor.offset.y = 0;
  scissor.extent.width = viewport.width;
  scissor.extent.height = viewport.height;
  m_devFuncs->vkCmdSetScissor(cb, 0, 1, &scissor);

  m_devFuncs->vkCmdDraw(cb, 3, 1, 0, 0);

  m_devFuncs->vkCmdEndRenderPass(cmdBuf);

  m_window->frameReady();
  m_window->requestUpdate();  // render continuously, throttled by the
                              // presentation rate
  return []() {};
}
