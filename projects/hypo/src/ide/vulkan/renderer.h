#ifndef _IDE_VULKAN_RENDERER_H
#define _IDE_VULKAN_RENDERER_H

#include <vulkan/vulkan.h>

#include <functional>
#include <optional>
#include <utility>
#include <variant>

class Renderer {
 public:
  struct Error {
    std::string msg;
  };
  template <typename T>
  using ErrorOr = std::variant<Error, T>;

  static ErrorOr<Renderer> Create(VkInstance instance,
                                  VkPhysicalDevice physical_device,
                                  VkDevice device);
  class MemoryAllocator {
   public:
    class Allocation {
     public:
      Allocation(VkDevice device, VkDeviceMemory memory)
          : device_(device), memory_(memory) {}
      ~Allocation();

      VkDeviceMemory memory() const { return memory_; }

     private:
      VkDevice device_;
      VkDeviceMemory memory_;
    };

    MemoryAllocator(VkPhysicalDevice physical_device, VkDevice device);
    ErrorOr<std::unique_ptr<Allocation>> Allocate(
        VkBuffer buffer, VkMemoryPropertyFlags memory_property_flags);
    ErrorOr<std::unique_ptr<Allocation>> Allocate(
        VkDeviceSize size, uint32_t type_filter,
        VkMemoryPropertyFlags memory_property_flags);
    ErrorOr<std::unique_ptr<Allocation>> Allocate(VkDeviceSize size,
                                                  uint32_t memory_type);

   private:
    VkPhysicalDevice physical_device_;
    VkDevice device_;
    VkPhysicalDeviceMemoryProperties physical_device_memory_properties_;
  };

  ~Renderer();

  // TODO:
  //   1. Return a command buffer instead, it can be added to another as a sub
  //   command buffer.
  //   2. Return the set of resources used by the frame.
  //   3. Make the function accept an arbitrary polygon soup to render.
  std::function<void()> RenderFrame(
      VkCommandBuffer frame_command_buffer,
      const std::pair<uint32_t, uint32_t>& output_surface_size);

 private:
  struct RendererConstructorData {
    VkInstance instance;
    VkPhysicalDevice physical_device;
    VkDevice device;

    std::optional<MemoryAllocator> allocator;

    VkBuffer vertex_buffer = VK_NULL_HANDLE;
    std::unique_ptr<MemoryAllocator::Allocation> vertex_buffer_mem;

    VkDescriptorPool m_descPool = VK_NULL_HANDLE;
    VkDescriptorSetLayout m_descSetLayout = VK_NULL_HANDLE;

    VkPipelineCache m_pipelineCache = VK_NULL_HANDLE;
    VkPipelineLayout m_pipelineLayout = VK_NULL_HANDLE;
    VkPipeline m_pipeline = VK_NULL_HANDLE;

    // QMatrix4x4 m_proj;
    float m_rotation = 0.0f;
  };

  Renderer(RendererConstructorData data);

  RendererConstructorData data_;
};

#endif  // _IDE_VULKAN_RENDERER_H
