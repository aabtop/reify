#include "reify/typescript_cpp_v8/domain_visualizer_gui.h"

#include <vulkan/vulkan.h>

#include "backends/imgui_impl_vulkan.h"
#include "imgui.h"

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
  GuiLayer();
  ~GuiLayer();

  DomainVisualizer::ErrorOr<DomainVisualizer::Renderer::FrameResources> Render(
      VkCommandBuffer command_buffer, VkFramebuffer framebuffer,
      const std::array<uint32_t, 2>& output_surface_size);

  static DomainVisualizer::ErrorOr<std::unique_ptr<GuiLayer>> Create(
      VkInstance instance, VkPhysicalDevice physical_device, VkDevice device,
      VkFormat output_image_format);

 private:
};

GuiLayer::GuiLayer() {}

GuiLayer::~GuiLayer() { ImGui::DestroyContext(); }

DomainVisualizer::ErrorOr<DomainVisualizer::Renderer::FrameResources>
GuiLayer::Render(VkCommandBuffer command_buffer, VkFramebuffer framebuffer,
                 const std::array<uint32_t, 2>& output_surface_size) {
  ImGuiIO& io = ImGui::GetIO();
  io.DisplaySize = ImVec2(output_surface_size[0], output_surface_size[1]);
  io.DisplayFramebufferScale = ImVec2(1.0f, 1.0f);
  return DomainVisualizer::Error{"nope not implemented."};
}

DomainVisualizer::ErrorOr<std::unique_ptr<GuiLayer>> GuiLayer::Create(
    VkInstance instance, VkPhysicalDevice physical_device, VkDevice device,
    VkFormat output_image_format) {
  // Setup Dear ImGui context
  IMGUI_CHECKVERSION();
  ImGui::CreateContext();

  // Setup Dear ImGui style
  ImGui::StyleColorsDark();

  ImGui_ImplVulkan_InitInfo init_info = {};
  VkRenderPass render_pass;
  ImGui_ImplVulkan_Init(&init_info, render_pass);

  return std::make_unique<GuiLayer>();
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
    return wrapped_->RenderFrame(command_buffer, framebuffer,
                                 output_surface_size);
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
    return *error;
  }

  return std::make_unique<RendererGui>(
      std::move(std::get<1>(error_or_gui_layer)),
      std::move(std::get<1>(error_or_wrapped_renderer)));
}

}  // namespace typescript_cpp_v8
}  // namespace reify