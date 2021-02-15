#include <QVBoxLayout>
#include <QVulkanInstance>
#include <QVulkanWindow>

#include "src/ide/domain_visualizer_hypo.h"
#include "src/ide/domain_visualizer_qt.h"
#include "src/ide/vulkan/renderer.h"

class DomainVisualizerVulkanWindowRenderer : public QVulkanWindowRenderer {
 public:
  DomainVisualizerVulkanWindowRenderer(QVulkanWindow* window)
      : window_(window) {}

  void initResources() override {
    auto renderer_or_error = Renderer::Create(
        window_->vulkanInstance()->vkInstance(), window_->physicalDevice(),
        window_->device(), window_->colorFormat());
    if (auto error = std::get_if<Renderer::Error>(&renderer_or_error)) {
      qFatal("Error creating Vulkan renderer: %s", error->msg);
    }
    renderer_.emplace(std::move(std::get<Renderer>(renderer_or_error)));
  }

  void initSwapChainResources() override{};
  void releaseSwapChainResources() override{};
  void releaseResources() override { renderer_ = std::nullopt; };

  void startNextFrame() override {
    int current_frame_index = window_->currentFrame();

    if (current_frame_index >= frame_resources_.size()) {
      frame_resources_.resize(current_frame_index);
    }

    // Clean up previous frame's resources.
    frame_resources_[current_frame_index] = std::nullopt;

    QSize image_size = window_->swapChainImageSize();
    auto error_or_frame_resources = renderer_->RenderFrame(
        window_->currentCommandBuffer(), window_->currentFramebuffer(),
        {static_cast<uint32_t>(image_size.width()),
         static_cast<uint32_t>(image_size.height())});
    if (auto error = std::get_if<Renderer::Error>(&error_or_frame_resources)) {
      qFatal("Vulkan error while rendering frame: %s", error->msg);
    }

    frame_resources_[current_frame_index] =
        std::move(std::get<Renderer::FrameResources>(error_or_frame_resources));

    window_->frameReady();
    // render continuously, throttled by the presentation rate.
    window_->requestUpdate();
  };

 private:
  QVulkanWindow* window_;
  std::optional<Renderer> renderer_;

  std::vector<std::optional<Renderer::FrameResources>> frame_resources_;
};

class DomainVisualizerVulkanWindow : public QVulkanWindow {
 public:
  QVulkanWindowRenderer* createRenderer() override {
    return new DomainVisualizerVulkanWindowRenderer(this);
  }
};

class DomainVisualizerQtVulkan : public DomainVisualizer {
 public:
  DomainVisualizerQtVulkan(QWidget* frame) {
    q_vulkan_instance_.setLayers(QByteArrayList()
                                 << "VK_LAYER_LUNARG_standard_validation");
    if (!q_vulkan_instance_.create())
      qFatal("Failed to create Vulkan instance: %d",
             q_vulkan_instance_.errorCode());

    auto vulkan_window = new DomainVisualizerVulkanWindow();
    vulkan_window->setVulkanInstance(&q_vulkan_instance_);

    vulkan_window_widget_.reset(
        QWidget::createWindowContainer(vulkan_window, frame));

    QVBoxLayout* layout = new QVBoxLayout();
    layout->addWidget(vulkan_window_widget_.get());
    frame->setLayout(layout);

    wrapped_domain_visualizer_.reset(new DomainVisualizerHypo());
  }

  std::vector<reify::CompilerEnvironment::InputModule> GetTypeScriptModules()
      override {
    return wrapped_domain_visualizer_->GetTypeScriptModules();
  }

  bool CanConsumeSymbol(
      const reify::CompiledModule::ExportedSymbol& symbol) override {
    return wrapped_domain_visualizer_->CanConsumeSymbol(symbol);
  }

  void ConsumeSymbol(std::shared_ptr<reify::CompiledModule> module,
                     const reify::CompiledModule::ExportedSymbol& symbol,
                     const std::function<void(std::optional<ConsumeError>&&)>&
                         on_consumed) override {
    return wrapped_domain_visualizer_->ConsumeSymbol(module, symbol,
                                                     std::move(on_consumed));
  }

 private:
  QVulkanInstance q_vulkan_instance_;
  std::unique_ptr<QWidget>
      vulkan_window_widget_;  // This is a unique_ptr to ensure it is destroyed
                              // before the QVulkanInstance object.

  std::unique_ptr<DomainVisualizer> wrapped_domain_visualizer_;
};

std::unique_ptr<DomainVisualizer> CreateDefaultQtWidgetDomainVisualizer(
    QWidget* frame) {
  return std::make_unique<DomainVisualizerQtVulkan>(frame);
}
