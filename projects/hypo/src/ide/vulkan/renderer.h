#ifndef _IDE_VULKAN_RENDERER_H
#define _IDE_VULKAN_RENDERER_H

#include <vulkan/vulkan.h>

#include <functional>
#include <memory>
#include <optional>
#include <utility>
#include <variant>

template <typename T>
class WithDeleter {
 public:
  WithDeleter(T&& value, const std::function<void(T&&)>& deleter)
      : value_(std::move(value)), deleter_(deleter) {}
  WithDeleter(WithDeleter&&) = default;
  WithDeleter(const WithDeleter&) = delete;
  WithDeleter& operator=(const WithDeleter&) = delete;

  // Move an existing value but use a different deleter.
  WithDeleter(WithDeleter&& other, const std::function<void(T&&)>& deleter)
      : value_(std::move(other.value_)), deleter_(deleter) {}

  const T& value() const { return value_; }

 private:
  T value_;
  std::function<void(T&&)> deleter_;
};

class Renderer {
 public:
  struct Error {
    std::string msg;
  };
  template <typename T>
  using ErrorOr = std::variant<Error, T>;

  static ErrorOr<Renderer> Create(VkInstance instance,
                                  VkPhysicalDevice physical_device,
                                  VkDevice device,
                                  VkFormat output_image_format);

  Renderer(Renderer&& other) = default;
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

    WithDeleter<VkBuffer> vertex_buffer;

    WithDeleter<VkDescriptorPool> descriptor_pool;
    WithDeleter<VkDescriptorSetLayout> descriptor_set_layout;

    WithDeleter<VkPipelineCache> pipeline_cache;
    WithDeleter<VkPipelineLayout> pipeline_layout;
    WithDeleter<VkPipeline> pipeline;
  };

  Renderer(RendererConstructorData&& data) : data_(std::move(data)) {}

  RendererConstructorData data_;
};

#endif  // _IDE_VULKAN_RENDERER_H
