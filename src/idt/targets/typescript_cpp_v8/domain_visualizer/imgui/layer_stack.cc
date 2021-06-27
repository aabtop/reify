#include "reify/typescript_cpp_v8/imgui/layer_stack.h"

#include <fmt/format.h>
#include <vulkan/vulkan.h>

#include <iostream>

#include "backends/imgui_impl_vulkan.h"
#include "imgui.h"
#include "imgui_internal.h"
#include "platform_window/platform_window_key.h"
#include "vulkan_utils/vulkan_utils.h"

namespace reify {
namespace typescript_cpp_v8 {
namespace imgui {

namespace {
// ImGui kind of forces us to use global variables :(.
VkResult* GetImGuiVulkanResult() {
  static VkResult result = VK_SUCCESS;
  return &result;
}
}  // namespace

LayerStack::LayerStack(const std::vector<Layer>& layers) : layers_(layers) {
  // Setup Dear ImGui context
  IMGUI_CHECKVERSION();
  ImGui::CreateContext();

  // Setup Dear ImGui style
  ImGui::StyleColorsDark();
  ImGuiIO& io = ImGui::GetIO();
  // We set this to null to not save/restore UI layouts from run to run.
  // Not sure if this is the best thing to do, but I'm preferring to err on
  // the side of no IO.
  io.IniFilename = nullptr;

  io.ConfigFlags |=
      ImGuiConfigFlags_DockingEnable | ImGuiConfigFlags_NavEnableKeyboard;

  // Keyboard mapping. Dear ImGui will use those indices to peek into the
  // io.KeysDown[] array.
  io.KeyMap[ImGuiKey_Tab] = kPlatformWindowKeyTab;
  io.KeyMap[ImGuiKey_LeftArrow] = kPlatformWindowKeyLeft;
  io.KeyMap[ImGuiKey_RightArrow] = kPlatformWindowKeyRight;
  io.KeyMap[ImGuiKey_UpArrow] = kPlatformWindowKeyUp;
  io.KeyMap[ImGuiKey_DownArrow] = kPlatformWindowKeyDown;
  io.KeyMap[ImGuiKey_PageUp] = kPlatformWindowKeyPrior;
  io.KeyMap[ImGuiKey_PageDown] = kPlatformWindowKeyNext;
  io.KeyMap[ImGuiKey_Home] = kPlatformWindowKeyHome;
  io.KeyMap[ImGuiKey_End] = kPlatformWindowKeyEnd;
  io.KeyMap[ImGuiKey_Insert] = kPlatformWindowKeyInsert;
  io.KeyMap[ImGuiKey_Delete] = kPlatformWindowKeyDelete;
  io.KeyMap[ImGuiKey_Backspace] = kPlatformWindowKeyBackspace;
  io.KeyMap[ImGuiKey_Space] = kPlatformWindowKeySpace;
  io.KeyMap[ImGuiKey_Enter] = kPlatformWindowKeyReturn;
  io.KeyMap[ImGuiKey_Escape] = kPlatformWindowKeyEscape;
  io.KeyMap[ImGuiKey_A] = kPlatformWindowKeyA;
  io.KeyMap[ImGuiKey_C] = kPlatformWindowKeyC;
  io.KeyMap[ImGuiKey_V] = kPlatformWindowKeyV;
  io.KeyMap[ImGuiKey_X] = kPlatformWindowKeyX;
  io.KeyMap[ImGuiKey_Y] = kPlatformWindowKeyY;
  io.KeyMap[ImGuiKey_Z] = kPlatformWindowKeyZ;
}

LayerStack::~LayerStack() { ImGui::DestroyContext(); }

bool LayerStack::OnInputEvent(const InputEvent& input_event) {
  ImGuiIO& io = ImGui::GetIO();

  if (auto event = std::get_if<MouseMoveEvent>(&input_event)) {
    mouse_pos_ = {event->x, event->y};
  } else if (auto event = std::get_if<MouseButtonEvent>(&input_event)) {
    auto to_imgui_button = [](MouseButton button) -> int {
      switch (button) {
        case MouseButton::Left:
          return 0;
        case MouseButton::Right:
          return 1;
        default:
          return -1;
      }
    };
    int imgui_button = to_imgui_button(event->button);
    if (imgui_button != -1) {
      io.MouseDown[imgui_button] = event->pressed;

      if (io.WantCaptureMouse && event->pressed) {
        // Only prevent the event from being passed on to the wrapped
        // visualizer if it was a press event and ImGui really wanted to
        // capture it.
        return false;
      }
    }
  } else if (auto event = std::get_if<MouseWheelEvent>(&input_event)) {
    if (io.WantCaptureMouse) {
      io.MouseWheel += event->angle_in_degrees / 15.0f;
      return false;
    }
  } else if (auto event = std::get_if<KeyboardEvent>(&input_event)) {
    io.KeysDown[event->key] = event->pressed;
    io.KeyCtrl = io.KeysDown[kPlatformWindowKeyControl];
    io.KeyShift = io.KeysDown[kPlatformWindowKeyShift];
    io.KeyAlt = io.KeysDown[kPlatformWindowKeyMenu];
    io.KeySuper = io.KeysDown[kPlatformWindowKeyLwin] ||
                  io.KeysDown[kPlatformWindowKeyRwin];

    // This is really not great at all, we should really setup platform_window
    // to hook into the OS's character generation system.
    if (event->pressed) {
      auto map_key_to_char = [](PlatformWindowKey key,
                                bool capitalize) -> std::optional<char> {
        if (key >= kPlatformWindowKeyA && key <= kPlatformWindowKeyZ) {
          return key - kPlatformWindowKeyA + (capitalize ? 'A' : 'a');
        } else {
          switch (key) {
            case kPlatformWindowKeyOemMinus:
              return (capitalize ? '_' : '-');
            case kPlatformWindowKeyOemPeriod:
              return (capitalize ? '>' : '.');
            case kPlatformWindowKeyOemComma:
              return (capitalize ? '<' : ',');
            case kPlatformWindowKeySpace:
              return ' ';
            case kPlatformWindowKeyBackspace:
              return '\b';
            case kPlatformWindowKeyDelete:
              return 127;
            case kPlatformWindowKeyReturn:
              return '\r';
            default:
              return std::nullopt;
          }
        }
      };
      auto maybe_char = map_key_to_char(
          static_cast<PlatformWindowKey>(event->key), io.KeyShift);
      if (maybe_char) {
        char buffer[2];
        buffer[1] = '\0';
        buffer[0] = *maybe_char;
        io.AddInputCharactersUTF8(buffer);
      }
    }

    if (io.WantCaptureKeyboard && event->pressed) {
      // Only prevent the event from being passed on to the wrapped visualizer
      // if it was a press event and ImGui really wanted to capture it.
      return false;
    }
    // Not implemented!
  }

  return true;
}

void LayerStack::OnViewportResize(const std::array<int, 2>& size) {
  size_ = size;
}

void LayerStack::AdvanceTime(std::chrono::duration<float> seconds) {
  accumulated_time_since_last_render_ += seconds;
}

class RendererImGui : public window::Window::Renderer {
 public:
  struct VulkanConstructorData {
    VkInstance instance;
    VkPhysicalDevice physical_device;
    VkDevice device;

    vulkan_utils::WithDeleter<VkPipelineCache> pipeline_cache;
    vulkan_utils::WithDeleter<VkDescriptorPool> descriptor_pool;
    vulkan_utils::WithDeleter<VkRenderPass> render_pass;
  };

  static vulkan_utils::ErrorOr<VulkanConstructorData>
  CreateVulkanConstructorData(VkInstance instance,
                              VkPhysicalDevice physical_device, VkDevice device,
                              VkFormat output_image_format);
  RendererImGui(VulkanConstructorData&& data,
                const std::vector<LayerStack::Layer>& layers,
                std::chrono::duration<float>* time_since_last_render,
                LayerStack* parent)
      : vulkan_constructor_data_(std::move(data)),
        parent_(parent),
        layers_(layers),
        time_since_last_render_(time_since_last_render) {}
  ~RendererImGui() { ImGui_ImplVulkan_Shutdown(); }

  ErrorOr<FrameResources> RenderFrame(
      VkCommandBuffer command_buffer, VkFramebuffer framebuffer,
      VkImage output_color_image, const window::Rect& viewport_region) override;

 private:
  VulkanConstructorData vulkan_constructor_data_;
  LayerStack* parent_;

  std::vector<LayerStack::Layer> layers_;

  bool created_imgui_fonts_texture_ = false;

  std::chrono::duration<float>* time_since_last_render_;
};

window::Window::ErrorOr<std::unique_ptr<window::Window::Renderer>>
LayerStack::CreateRenderer(VkInstance instance,
                           VkPhysicalDevice physical_device, VkDevice device,
                           VkFormat output_image_format) {
  auto error_or_vulkan_constructor_data =
      RendererImGui::CreateVulkanConstructorData(instance, physical_device,
                                                 device, output_image_format);
  if (auto error = std::get_if<0>(&error_or_vulkan_constructor_data)) {
    return Error{error->msg};
  }

  return std::make_unique<RendererImGui>(
      std::move(std::get<1>(error_or_vulkan_constructor_data)), layers_,
      &accumulated_time_since_last_render_, this);
}

vulkan_utils::ErrorOr<RendererImGui::VulkanConstructorData>
RendererImGui::CreateVulkanConstructorData(VkInstance instance,
                                           VkPhysicalDevice physical_device,
                                           VkDevice device,
                                           VkFormat output_image_format) {
  int32_t queue_family = -1;
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
    if (queue_family == -1) {
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
      *GetImGuiVulkanResult() = result;
    }
  };

  VULKAN_UTILS_ASSIGN_OR_RETURN(
      render_pass,
      vulkan_utils::MakeRenderPass(
          device,
          {VkAttachmentDescription{
              0,
              output_image_format,
              VK_SAMPLE_COUNT_1_BIT,
              VK_ATTACHMENT_LOAD_OP_LOAD,
              VK_ATTACHMENT_STORE_OP_STORE,
              VK_ATTACHMENT_LOAD_OP_DONT_CARE,
              VK_ATTACHMENT_STORE_OP_DONT_CARE,
              VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL,
              VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL,
          }},
          VkAttachmentDescription{
              0, VK_FORMAT_D32_SFLOAT, VK_SAMPLE_COUNT_1_BIT,
              VK_ATTACHMENT_LOAD_OP_DONT_CARE, VK_ATTACHMENT_STORE_OP_DONT_CARE,
              VK_ATTACHMENT_LOAD_OP_DONT_CARE, VK_ATTACHMENT_STORE_OP_DONT_CARE,
              VK_IMAGE_LAYOUT_UNDEFINED,
              VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL}));

  ImGui_ImplVulkan_Init(&init_info, render_pass.value());
  if (*GetImGuiVulkanResult() != VK_SUCCESS) {
    return vulkan_utils::Error{
        fmt::format("ImGui encountered a Vulkan error while initializing: {}",
                    *GetImGuiVulkanResult())};
  }

  return VulkanConstructorData{
      instance,
      physical_device,
      device,
      std::move(pipeline_cache),
      std::move(descriptor_pool),
      std::move(render_pass),
  };
}

window::Window::ErrorOr<window::Window::Renderer::FrameResources>
RendererImGui::RenderFrame(VkCommandBuffer command_buffer,
                           VkFramebuffer framebuffer,
                           VkImage output_color_image,
                           const window::Rect& viewport_region) {
  ImGuiIO& io = ImGui::GetIO();
  io.DisplaySize = ImVec2(viewport_region.width(), viewport_region.height());
  io.DisplayFramebufferScale = ImVec2(1.0f, 1.0f);
  io.DeltaTime = std::max(time_since_last_render_->count(), 0.000000001f);
  float window_scale =
      parent_->size_ ? viewport_region.width() / (*parent_->size_)[0] : 1.0f;
  // Sometimes the window events are reported on a different scale than the
  // rendering surface, but ImGui assumes these are equal. So, in this case,
  // we make sure to transform our event coordinates into rendering coordinates
  // before passing them into ImGui.
  if (parent_->mouse_pos_) {
    io.MousePos = ImVec2((*parent_->mouse_pos_)[0] * window_scale,
                         (*parent_->mouse_pos_)[1] * window_scale);
  }
  io.FontGlobalScale = window_scale;
  *time_since_last_render_ = std::chrono::duration<float>::zero();

  if (!created_imgui_fonts_texture_) {
    created_imgui_fonts_texture_ = true;
    ImGui_ImplVulkan_CreateFontsTexture(command_buffer);
  }

  ImGui_ImplVulkan_NewFrame();
  if (*GetImGuiVulkanResult() != VK_SUCCESS) {
    return vulkan_utils::Error{
        fmt::format("ImGui encountered a Vulkan error while initializing: {}",
                    *GetImGuiVulkanResult())};
  }

  ImGui::NewFrame();

  for (const auto& im_gui_layer : layers_) {
    im_gui_layer();
  }

  ImGui::Render();
  if (*GetImGuiVulkanResult() != VK_SUCCESS) {
    return vulkan_utils::Error{
        fmt::format("ImGui encountered a Vulkan error while initializing: {}",
                    *GetImGuiVulkanResult())};
  }

  VkRenderPassBeginInfo rpb{};
  rpb.sType = VK_STRUCTURE_TYPE_RENDER_PASS_BEGIN_INFO;
  rpb.renderPass = vulkan_constructor_data_.render_pass.value();
  rpb.framebuffer = framebuffer;
  rpb.renderArea.offset.x = viewport_region.left;
  rpb.renderArea.offset.y = viewport_region.top;
  rpb.renderArea.extent.width = viewport_region.width();
  rpb.renderArea.extent.height = viewport_region.height();
  rpb.clearValueCount = 0;
  rpb.pClearValues = nullptr;

  vkCmdBeginRenderPass(command_buffer, &rpb, VK_SUBPASS_CONTENTS_INLINE);

  ImGui_ImplVulkan_RenderDrawData(ImGui::GetDrawData(), command_buffer);

  vkCmdEndRenderPass(command_buffer);

  return window::Window::Renderer::FrameResources();
}

}  // namespace imgui
}  // namespace typescript_cpp_v8
}  // namespace reify