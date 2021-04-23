#include "reify/typescript_cpp_v8/domain_visualizer_gui.h"

#include <vulkan/vulkan.h>

#include <iostream>

#include "backends/imgui_impl_vulkan.h"
#include "imgui.h"
#include "vulkan_utils/vulkan_utils.h"

namespace reify {
namespace typescript_cpp_v8 {

DomainVisualizerGui::DomainVisualizerGui(
    std::unique_ptr<DomainVisualizer> wrapped)
    : wrapped_(std::move(wrapped)) {}

std::vector<reify::CompilerEnvironment::InputModule>
DomainVisualizerGui::GetTypeScriptModules() {
  return wrapped_->GetTypeScriptModules();
}

bool DomainVisualizerGui::CanPreviewSymbol(
    const reify::CompiledModule::ExportedSymbol& symbol) {
  return wrapped_->CanPreviewSymbol(symbol);
}

void DomainVisualizerGui::PrepareSymbolForPreview(
    std::shared_ptr<reify::CompiledModule> module,
    const reify::CompiledModule::ExportedSymbol& symbol,
    const std::function<void(ErrorOr<PreparedSymbol>)>& on_preview_prepared) {
  wrapped_->PrepareSymbolForPreview(module, symbol, on_preview_prepared);
}

void DomainVisualizerGui::Preview(const PreparedSymbol& prepared_symbol) {
  wrapped_->Preview(prepared_symbol);
}

void DomainVisualizerGui::OnInputEvent(const InputEvent& input_event) {
  wrapped_->OnInputEvent(input_event);
}

void DomainVisualizerGui::OnViewportResize(const std::array<int, 2>& size) {
  wrapped_->OnViewportResize(size);
}

void DomainVisualizerGui::AdvanceTime(std::chrono::duration<float> seconds) {
  wrapped_->AdvanceTime(seconds);
}

namespace {

class GuiLayer {
 public:
  ~GuiLayer();

  vulkan_utils::ErrorOr<DomainVisualizer::Renderer::FrameResources> Render(
      VkCommandBuffer command_buffer, VkFramebuffer framebuffer,
      const std::array<uint32_t, 2>& output_surface_size);

  static vulkan_utils::ErrorOr<std::unique_ptr<GuiLayer>> Create(
      VkInstance instance, VkPhysicalDevice physical_device, VkDevice device,
      VkFormat output_image_format);

 private:
  struct ConstructorData {
    VkInstance instance;
    VkPhysicalDevice physical_device;
    VkDevice device;

    vulkan_utils::WithDeleter<VkPipelineCache> pipeline_cache;
    vulkan_utils::WithDeleter<VkDescriptorPool> descriptor_pool;
    vulkan_utils::WithDeleter<VkRenderPass> render_pass;
  };
  GuiLayer(ConstructorData&& data) : data_(std::move(data)) {}

  ConstructorData data_;
};

GuiLayer::~GuiLayer() {
  ImGui_ImplVulkan_Shutdown();
  ImGui::DestroyContext();
}

vulkan_utils::ErrorOr<DomainVisualizer::Renderer::FrameResources>
GuiLayer::Render(VkCommandBuffer command_buffer, VkFramebuffer framebuffer,
                 const std::array<uint32_t, 2>& output_surface_size) {
  ImGuiIO& io = ImGui::GetIO();
  io.DisplaySize = ImVec2(output_surface_size[0], output_surface_size[1]);
  io.DisplayFramebufferScale = ImVec2(1.0f, 1.0f);

  return vulkan_utils::Error{"nope not implemented."};
}

vulkan_utils::ErrorOr<std::unique_ptr<GuiLayer>> GuiLayer::Create(
    VkInstance instance, VkPhysicalDevice physical_device, VkDevice device,
    VkFormat output_image_format) {
  // Setup Dear ImGui context
  IMGUI_CHECKVERSION();
  ImGui::CreateContext();

  // Setup Dear ImGui style
  ImGui::StyleColorsDark();

  uint32_t queue_family;
  {
    uint32_t count;
    vkGetPhysicalDeviceQueueFamilyProperties(physical_device, &count, NULL);
    std::vector<VkQueueFamilyProperties> queues(count);

    vkGetPhysicalDeviceQueueFamilyProperties(physical_device, &count,
                                             queues.data());
    for (uint32_t i = 0; i < count; i++) {
      if (queues[i].queueFlags & VK_QUEUE_GRAPHICS_BIT) {
        queue_family = i;
        break;
      }
    }
    if (queue_family == count) {
      return vulkan_utils::Error{"Could not find a Vulkan graphics queue."};
    }
  }

  VkQueue queue;
  vkGetDeviceQueue(device, queue_family, 0, &queue);

  VULKAN_UTILS_ASSIGN_OR_RETURN(pipeline_cache,
                                vulkan_utils::MakePipelineCache(device));
  VULKAN_UTILS_ASSIGN_OR_RETURN(
      descriptor_pool,
      vulkan_utils::MakeDescriptorPool(
          device,
          {VkDescriptorPoolSize{VK_DESCRIPTOR_TYPE_SAMPLER, 1000},
           VkDescriptorPoolSize{VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER,
                                1000},
           VkDescriptorPoolSize{VK_DESCRIPTOR_TYPE_SAMPLED_IMAGE, 1000},
           VkDescriptorPoolSize{VK_DESCRIPTOR_TYPE_STORAGE_IMAGE, 1000},
           VkDescriptorPoolSize{VK_DESCRIPTOR_TYPE_UNIFORM_TEXEL_BUFFER, 1000},
           VkDescriptorPoolSize{VK_DESCRIPTOR_TYPE_STORAGE_TEXEL_BUFFER, 1000},
           VkDescriptorPoolSize{VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER, 1000},
           VkDescriptorPoolSize{VK_DESCRIPTOR_TYPE_STORAGE_BUFFER, 1000},
           VkDescriptorPoolSize{VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER_DYNAMIC,
                                1000},
           VkDescriptorPoolSize{VK_DESCRIPTOR_TYPE_STORAGE_BUFFER_DYNAMIC,
                                1000},
           VkDescriptorPoolSize{VK_DESCRIPTOR_TYPE_INPUT_ATTACHMENT, 1000}},
          1));

  ImGui_ImplVulkan_InitInfo init_info = {};
  init_info.Instance = instance;
  init_info.PhysicalDevice = physical_device;
  init_info.Device = device;
  init_info.QueueFamily = queue_family;
  init_info.Queue = queue;
  init_info.PipelineCache = pipeline_cache.value();
  init_info.DescriptorPool = descriptor_pool.value();
  init_info.Allocator = nullptr;
  init_info.MinImageCount =
      2;  // Not clear what this is really needed for, and since we don't know
          // our framebuffer yet we can't query it directly.
  init_info.ImageCount = 2;
  init_info.CheckVkResultFn = [](VkResult result) {
    if (result != VK_SUCCESS) {
      std::cerr << "Error " << result
                << " from ImGui, but error handling isn't properly hooked up "
                   "here yet."
                << std::endl;
    }
  };

  VULKAN_UTILS_ASSIGN_OR_RETURN(
      render_pass,
      vulkan_utils::MakeRenderPass(
          device,
          {VkAttachmentDescription{
              0, output_image_format, VK_SAMPLE_COUNT_1_BIT,
              VK_ATTACHMENT_LOAD_OP_LOAD, VK_ATTACHMENT_STORE_OP_STORE,
              VK_ATTACHMENT_LOAD_OP_DONT_CARE, VK_ATTACHMENT_STORE_OP_DONT_CARE,
              VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL,
              VK_IMAGE_LAYOUT_PRESENT_SRC_KHR,  // VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL?
          }},
          std::nullopt));

  ImGui_ImplVulkan_Init(&init_info, render_pass.value());

  return std::unique_ptr<GuiLayer>(new GuiLayer(ConstructorData{
      instance,
      physical_device,
      device,
      std::move(pipeline_cache),
      std::move(descriptor_pool),
      std::move(render_pass),
  }));
}

class RendererGui : public DomainVisualizer::Renderer {
 public:
  RendererGui(std::unique_ptr<GuiLayer> gui_layer,
              std::unique_ptr<Renderer> wrapped)
      : gui_layer_(std::move(gui_layer)), wrapped_(std::move(wrapped)) {}
  ~RendererGui() {}

  ErrorOr<FrameResources> RenderFrame(
      VkCommandBuffer command_buffer, VkFramebuffer framebuffer,
      const std::array<uint32_t, 2>& output_surface_size) override {
    VULKAN_UTILS_ASSIGN_OR_RETURN(
        wrapped_resources, wrapped_->RenderFrame(command_buffer, framebuffer,
                                                 output_surface_size));
    // We can't use the VULKAN_UTILS_ASSIGN_OR_RETURN here because the error
    // type is subtly different (`vulkan_utils::Error` versus
    // `DomainVisualizer::Error`), so the error would actually get stuffed into
    // the `std::any`.
    auto maybe_gui_layer_resources =
        gui_layer_->Render(command_buffer, framebuffer, output_surface_size);
    if (auto error = std::get_if<0>(&maybe_gui_layer_resources)) {
      return DomainVisualizer::Error{error->msg};
    }

    return std::pair{wrapped_resources, std::get<1>(maybe_gui_layer_resources)};
  }

 private:
  std::unique_ptr<GuiLayer> gui_layer_;
  std::unique_ptr<Renderer> wrapped_;
};

}  // namespace

DomainVisualizer::ErrorOr<std::unique_ptr<DomainVisualizerGui::Renderer>>
DomainVisualizerGui::CreateRenderer(VkInstance instance,
                                    VkPhysicalDevice physical_device,
                                    VkDevice device,
                                    VkFormat output_image_format) {
  auto error_or_wrapped_renderer = wrapped_->CreateRenderer(
      instance, physical_device, device, output_image_format);
  if (auto error = std::get_if<0>(&error_or_wrapped_renderer)) {
    return *error;
  }

  auto error_or_gui_layer =
      GuiLayer::Create(instance, physical_device, device, output_image_format);
  if (auto error = std::get_if<0>(&error_or_gui_layer)) {
    return Error{error->msg};
  }

  return std::make_unique<RendererGui>(
      std::move(std::get<1>(error_or_gui_layer)),
      std::move(std::get<1>(error_or_wrapped_renderer)));
}

}  // namespace typescript_cpp_v8
}  // namespace reify