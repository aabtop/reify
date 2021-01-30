#include <QVBoxLayout>
#include <QVulkanInstance>
#include <QVulkanWindow>

#include "src/ide/domain_visualizer_hypo.h"
#include "src/ide/domain_visualizer_qt.h"
#include "src/ide/vulkan/triangle_renderer.h"

class DomainVisualizerVulkanWindow : public QVulkanWindow {
 public:
  QVulkanWindowRenderer* createRenderer() override {
    return new TriangleRenderer(this);
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
