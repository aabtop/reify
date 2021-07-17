#include "reify/typescript_cpp_v8/symbol_visualizer.h"

#include <fmt/format.h>

namespace reify {
namespace typescript_cpp_v8 {

namespace {
std::unordered_map<std::string, TypeScriptSymbolVisualizer> ToTypeVisualizerMap(
    const std::vector<TypeScriptSymbolVisualizer>& visualizers) {
  std::unordered_map<std::string, TypeScriptSymbolVisualizer> result;
  for (const auto& visualizer : visualizers) {
    result.insert(std::make_pair(visualizer.typescript_type, visualizer));
  }
  return result;
}
}  // namespace

SymbolVisualizer::SymbolVisualizer(
    const std::vector<CompilerEnvironment::InputModule>& typescript_modules,
    const std::vector<TypeScriptSymbolVisualizer>& visualizers)
    : typescript_modules_(typescript_modules),
      typescript_type_to_visualizer_(ToTypeVisualizerMap(visualizers)) {}

const TypeScriptSymbolVisualizer* SymbolVisualizer::FindVisualizerForSymbol(
    const CompiledModule::ExportedSymbol& symbol) const {
  for (const auto& item : typescript_type_to_visualizer_) {
    if (symbol.typescript_type_string == fmt::format("() => {}", item.first)) {
      return &item.second;
    }
  }
  return nullptr;
}

bool SymbolVisualizer::CanPreviewSymbol(
    const CompiledModule::ExportedSymbol& symbol) {
  return FindVisualizerForSymbol(symbol);
}

reify::utils::Future<utils::ErrorOr<std::any>>
SymbolVisualizer::PrepareSymbolForPreview(
    std::shared_ptr<CompiledModule> module,
    const CompiledModule::ExportedSymbol& symbol) {
  return runtime_thread_.EnqueueWithResult<
      utils::ErrorOr<std::any>>([this, module, symbol = std::move(symbol)]()
                                    -> utils::ErrorOr<std::any> {
    // Setup a V8 runtime environment around the CompiledModule.  This will
    // enable us to call exported functions and query exported values from
    // the module.
    auto runtime_env_or_error = CreateRuntimeEnvironment(module);
    if (auto error = std::get_if<0>(&runtime_env_or_error)) {
      return utils::Error{*error};
    }

    RuntimeEnvironment runtime_env(
        std::move(std::get<1>(runtime_env_or_error)));

    auto visualizer = FindVisualizerForSymbol(symbol);
    if (!visualizer) {
      return utils::Error{
          "Hypo's visualizer does not support this symbol type."};
    }

    auto results =
        visualizer
            ->prepare_symbol_for_preview_function(&runtime_env, module, symbol)
            .wait_and_get_results();

    if (std::holds_alternative<utils::CancelledFuture>(results)) {
      return utils::Error{"Cancelled."};
    }
    if (auto error = std::get_if<0>(&std::get<1>(results))) {
      return *error;
    }
    return PreparedSymbol{std::get<std::any>(std::get<1>(results)), visualizer};
  });
}

class SymbolVisualizer::Renderer : public reify::window::Window::Renderer {
 public:
  Renderer(std::unordered_map<
               std::string, std::unique_ptr<reify::window::Window::Renderer>>&&
               renderers,
           const std::function<void()>& on_destroy);
  ~Renderer() { on_destroy_(); }

  void SetCurrent(const std::optional<std::string>& current_renderer) {
    if (current_renderer) {
      auto found = renderers_.find(*current_renderer);
      assert(found != renderers_.end());
      current_renderer_ = found->second.get();
    } else {
      current_renderer_ = nullptr;
    }
  }

  utils::ErrorOr<FrameResources> RenderFrame(
      VkCommandBuffer command_buffer, VkFramebuffer framebuffer,
      VkImage output_color_image,
      const reify::window::Rect& viewport_region) override {
    if (current_renderer_) {
      return current_renderer_->RenderFrame(
          command_buffer, framebuffer, output_color_image, viewport_region);
    } else {
      // Clear the viewport if no symbol is selected.
      VkClearColorValue clear_color = {{0.11, 0.11, 0.11, 1}};

      VkImageSubresourceRange image_range = {};
      image_range.aspectMask = VK_IMAGE_ASPECT_COLOR_BIT;
      image_range.levelCount = 1;
      image_range.layerCount = 1;

      vkCmdClearColorImage(command_buffer, output_color_image,
                           VK_IMAGE_LAYOUT_GENERAL, &clear_color, 1,
                           &image_range);

      return FrameResources();
    }
  }

 private:
  const std::unordered_map<std::string,
                           std::unique_ptr<reify::window::Window::Renderer>>
      renderers_;
  const std::function<void()> on_destroy_;

  reify::window::Window::Renderer* current_renderer_ = nullptr;
};

void SymbolVisualizer::SetPreview(const std::any& prepared_symbol) {
  if (selected_symbol_) {
    selected_symbol_->associated_visualizer->set_preview_function(std::nullopt);
    if (renderer_) {
      renderer_->SetCurrent(std::nullopt);
    }
  }
  selected_symbol_ = std::any_cast<PreparedSymbol>(prepared_symbol);

  if (last_viewport_resize_) {
    selected_symbol_->associated_visualizer->window->OnViewportResize(
        *last_viewport_resize_);
  }

  selected_symbol_->associated_visualizer->set_preview_function(
      selected_symbol_->processed_data);
  if (renderer_) {
    renderer_->SetCurrent(
        selected_symbol_->associated_visualizer->typescript_type);
  }
}

void SymbolVisualizer::ClearPreview() {
  if (selected_symbol_) {
    selected_symbol_->associated_visualizer->set_preview_function(std::nullopt);
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
    std::unordered_map<std::string,
                       std::unique_ptr<reify::window::Window::Renderer>>&&
        renderers,
    const std::function<void()>& on_destroy)
    : renderers_(std::move(renderers)), on_destroy_(on_destroy) {}

utils::ErrorOr<std::unique_ptr<reify::window::Window::Renderer>>
SymbolVisualizer::CreateRenderer(VkInstance instance,
                                 VkPhysicalDevice physical_device,
                                 VkDevice device,
                                 VkFormat output_image_format) {
  std::unordered_map<std::string,
                     std::unique_ptr<reify::window::Window::Renderer>>
      renderers;
  for (const auto& item : typescript_type_to_visualizer_) {
    auto error_or_renderer = item.second.window->CreateRenderer(
        instance, physical_device, device, output_image_format);
    if (auto error = std::get_if<0>(&error_or_renderer)) {
      return *error;
    }
    renderers[item.first] = std::move(std::get<1>(error_or_renderer));
  }

  std::unique_ptr<Renderer> renderer(
      new Renderer(std::move(renderers), [this]() { renderer_ = nullptr; }));
  renderer_ = renderer.get();
  if (selected_symbol_) {
    renderer_->SetCurrent(
        selected_symbol_->associated_visualizer->typescript_type);
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
