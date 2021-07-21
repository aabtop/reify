#include "reify/typescript_cpp_v8/symbol_visualizer.h"

#include <fmt/format.h>

#include "vulkan_utils/vulkan_utils.h"

namespace reify {
namespace typescript_cpp_v8 {

SymbolVisualizer::SymbolVisualizer(
    const std::vector<CompilerEnvironment::InputModule>& typescript_modules,
    const std::vector<TypeScriptSymbolVisualizer>& visualizers)
    : typescript_modules_(typescript_modules), visualizers_(visualizers) {}

std::optional<size_t> SymbolVisualizer::FindVisualizerIndexForSymbol(
    const CompiledModule::ExportedSymbol& symbol) const {
  for (size_t i = 0; i < visualizers_.size(); ++i) {
    if (visualizers_[i].can_preview_symbol(symbol)) {
      return i;
    }
  }
  return std::nullopt;
}

bool SymbolVisualizer::CanPreviewSymbol(
    const CompiledModule::ExportedSymbol& symbol) {
  return FindVisualizerIndexForSymbol(symbol).has_value();
}
std::string SymbolVisualizer::VisualizerTypeScriptTypeForSymbol(
    const CompiledModule::ExportedSymbol& symbol) {
  auto visualizer_index = FindVisualizerIndexForSymbol(symbol);
  assert(visualizer_index);
  return visualizers_[*visualizer_index].typescript_type;
}

reify::utils::Future<utils::ErrorOr<std::any>>
SymbolVisualizer::PrepareSymbolForPreview(
    std::shared_ptr<CompiledModule> module,
    const CompiledModule::ExportedSymbol& symbol) {
  return runtime_thread_.EnqueueWithResult<utils::ErrorOr<std::any>>(
      [this, module, symbol = std::move(symbol)]() -> utils::ErrorOr<std::any> {
        // Setup a V8 runtime environment around the CompiledModule.  This will
        // enable us to call exported functions and query exported values from
        // the module.
        auto runtime_env_or_error = CreateRuntimeEnvironment(module);
        if (auto error = std::get_if<0>(&runtime_env_or_error)) {
          return utils::Error{*error};
        }

        RuntimeEnvironment runtime_env(
            std::move(std::get<1>(runtime_env_or_error)));

        std::optional<size_t> maybe_visualizer_index =
            FindVisualizerIndexForSymbol(symbol);
        if (!maybe_visualizer_index) {
          return utils::Error{
              "Hypo's visualizer does not support this symbol type."};
        }

        auto visualizer = &visualizers_[*maybe_visualizer_index];
        auto results =
            visualizer->prepare_symbol_for_preview(&runtime_env, module, symbol)
                .wait_and_get_results();

        if (std::holds_alternative<utils::CancelledFuture>(results)) {
          return utils::Error{"Cancelled."};
        }
        if (auto error = std::get_if<0>(&std::get<1>(results))) {
          return *error;
        }
        return PreparedSymbol{std::get<std::any>(std::get<1>(results)),
                              visualizer, *maybe_visualizer_index};
      });
}

class SymbolVisualizer::Renderer : public reify::window::Window::Renderer {
 public:
  Renderer(
      std::vector<std::unique_ptr<reify::window::Window::Renderer>>&& renderers,
      const std::function<void()>& on_destroy,
      const std::shared_ptr<vulkan_utils::WithDeleter<VkRenderPass>>&
          clear_render_pass);
  ~Renderer() { on_destroy_(); }

  void SetCurrent(const std::optional<size_t>& current_renderer_index) {
    if (current_renderer_index) {
      current_renderer_ = renderers_[*current_renderer_index].get();
    } else {
      current_renderer_ = nullptr;
    }
  }

  utils::ErrorOr<FrameResources> RenderFrame(
      VkCommandBuffer command_buffer, VkFramebuffer framebuffer,
      VkImage output_color_image,
      const reify::window::Rect& viewport_region) override;

 private:
  const std::vector<std::unique_ptr<reify::window::Window::Renderer>>
      renderers_;
  const std::function<void()> on_destroy_;

  // Used for clearing the screen.
  std::shared_ptr<vulkan_utils::WithDeleter<VkRenderPass>> clear_render_pass_;

  reify::window::Window::Renderer* current_renderer_ = nullptr;
};

void SymbolVisualizer::SetPreview(const std::any& prepared_symbol) {
  if (selected_symbol_) {
    selected_symbol_->associated_visualizer->set_preview(std::nullopt);
    if (renderer_) {
      renderer_->SetCurrent(std::nullopt);
    }
  }
  selected_symbol_ = std::any_cast<PreparedSymbol>(prepared_symbol);

  if (last_viewport_resize_) {
    selected_symbol_->associated_visualizer->window->OnViewportResize(
        *last_viewport_resize_);
  }

  selected_symbol_->associated_visualizer->set_preview(
      selected_symbol_->processed_data);
  if (renderer_) {
    renderer_->SetCurrent(selected_symbol_->associated_visualizer_index);
  }
}

void SymbolVisualizer::ClearPreview() {
  if (selected_symbol_) {
    selected_symbol_->associated_visualizer->set_preview(std::nullopt);
  }
  selected_symbol_ = std::nullopt;
}

bool SymbolVisualizer::OnInputEvent(const InputEvent& input_event) {
  if (selected_symbol_) {
    return selected_symbol_->associated_visualizer->window->OnInputEvent(
        input_event);
  } else {
    return false;
  }
}

void SymbolVisualizer::OnViewportResize(const std::array<int, 2>& size) {
  last_viewport_resize_ = size;
  if (selected_symbol_) {
    selected_symbol_->associated_visualizer->window->OnViewportResize(size);
  }
}

void SymbolVisualizer::AdvanceTime(std::chrono::duration<float> seconds) {
  if (selected_symbol_) {
    selected_symbol_->associated_visualizer->window->AdvanceTime(seconds);
  }
}

SymbolVisualizer::Renderer::Renderer(
    std::vector<std::unique_ptr<reify::window::Window::Renderer>>&& renderers,
    const std::function<void()>& on_destroy,
    const std::shared_ptr<vulkan_utils::WithDeleter<VkRenderPass>>&
        clear_render_pass)
    : renderers_(std::move(renderers)),
      on_destroy_(on_destroy),
      clear_render_pass_(clear_render_pass) {}

utils::ErrorOr<window::Window::Renderer::FrameResources>
SymbolVisualizer::Renderer::RenderFrame(
    VkCommandBuffer command_buffer, VkFramebuffer framebuffer,
    VkImage output_color_image, const reify::window::Rect& viewport_region) {
  if (current_renderer_) {
    return current_renderer_->RenderFrame(command_buffer, framebuffer,
                                          output_color_image, viewport_region);
  } else {
    std::array<VkClearValue, 2> clear_values{};
    memset(clear_values.data(), 0,
           sizeof(clear_values[0]) * clear_values.size());
    clear_values[0].color = {{0.11, 0.11, 0.11, 1}};
    clear_values[1].depthStencil = {1, 0};

    VkRenderPassBeginInfo rpb{};
    rpb.sType = VK_STRUCTURE_TYPE_RENDER_PASS_BEGIN_INFO;
    rpb.renderPass = clear_render_pass_->value();
    rpb.framebuffer = framebuffer;
    rpb.renderArea.offset.x = viewport_region.left;
    rpb.renderArea.offset.y = viewport_region.top;
    rpb.renderArea.extent.width = viewport_region.width();
    rpb.renderArea.extent.height = viewport_region.height();
    rpb.clearValueCount = clear_values.size();
    rpb.pClearValues = clear_values.data();

    // Begin and end a render pass just to clear the view.
    vkCmdBeginRenderPass(command_buffer, &rpb, VK_SUBPASS_CONTENTS_INLINE);
    vkCmdEndRenderPass(command_buffer);

    return FrameResources(clear_render_pass_);
  }
}

utils::ErrorOr<std::unique_ptr<reify::window::Window::Renderer>>
SymbolVisualizer::CreateRenderer(VkInstance instance,
                                 VkPhysicalDevice physical_device,
                                 VkDevice device,
                                 VkFormat output_image_format) {
  std::vector<std::unique_ptr<reify::window::Window::Renderer>> renderers;
  for (const auto& visualizer : visualizers_) {
    auto error_or_renderer = visualizer.window->CreateRenderer(
        instance, physical_device, device, output_image_format);
    if (auto error = std::get_if<0>(&error_or_renderer)) {
      return *error;
    }
    renderers.push_back(std::move(std::get<1>(error_or_renderer)));
  }

  // Setup a render pass for when nothing is selected... It's possible we just
  // use this to issue a clear.
  auto error_or_render_pass = vulkan_utils::MakeRenderPass(
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
      });
  if (auto error = std::get_if<0>(&error_or_render_pass)) {
    return utils::Error{error->msg};
  }

  std::unique_ptr<Renderer> renderer(new Renderer(
      std::move(renderers), [this]() { renderer_ = nullptr; },
      std::make_shared<vulkan_utils::WithDeleter<VkRenderPass>>(
          std::move(std::get<1>(error_or_render_pass)))));
  renderer_ = renderer.get();
  if (selected_symbol_) {
    renderer_->SetCurrent(selected_symbol_->associated_visualizer_index);
  }
  return std::move(renderer);
}

bool SymbolVisualizer::HasImGuiWindow() const {
  return selected_symbol_ &&
         selected_symbol_->associated_visualizer->im_gui_visualizer;
}

std::string SymbolVisualizer::ImGuiWindowPanelTitle() const {
  if (selected_symbol_) {
    return selected_symbol_->associated_visualizer->im_gui_visualizer
        ->ImGuiWindowPanelTitle();
  } else {
    assert(false);
    return "nothing!";
  }
}

void SymbolVisualizer::RenderImGuiWindow() {
  if (selected_symbol_) {
    selected_symbol_->associated_visualizer->im_gui_visualizer
        ->RenderImGuiWindow();
  }
}

}  // namespace typescript_cpp_v8
}  // namespace reify
