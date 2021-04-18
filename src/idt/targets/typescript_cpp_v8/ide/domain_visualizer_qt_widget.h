#ifndef _SRC_IDT_TARGETS_TYPESCRIPT_CPP_V8_IDE_DOMAIN_VISUALIZER_QT_WIDGET_H
#define _SRC_IDT_TARGETS_TYPESCRIPT_CPP_V8_IDE_DOMAIN_VISUALIZER_QT_WIDGET_H

#include <QVulkanInstance>
#include <QVulkanWindow>
#include <QWidget>
#include <memory>

#include "reify/typescript_cpp_v8/domain_visualizer.h"

namespace reify {
namespace typescript_cpp_v8 {
namespace ide {

class DomainVisualizerVulkanWindowRenderer : public QVulkanWindowRenderer {
 public:
  DomainVisualizerVulkanWindowRenderer(QVulkanWindow* window,
                                       DomainVisualizer* domain_visualizer)
      : domain_visualizer_(domain_visualizer), window_(window) {}

  void initResources() override;

  void initSwapChainResources() override{};
  void releaseSwapChainResources() override{};
  void releaseResources() override;

  void startNextFrame() override;

 private:
  DomainVisualizer* domain_visualizer_;

  QVulkanWindow* window_;
  std::unique_ptr<DomainVisualizer::Renderer> domain_visualizer_renderer_;

  std::vector<std::optional<DomainVisualizer::Renderer::FrameResources>>
      frame_resources_;

  std::optional<std::chrono::time_point<std::chrono::high_resolution_clock>>
      last_render_time_;
};

class DomainVisualizerVulkanWindow : public QVulkanWindow {
  Q_OBJECT

 public:
  DomainVisualizerVulkanWindow(DomainVisualizer* domain_visualizer);
  QVulkanWindowRenderer* createRenderer() override;

 protected:
  void resizeEvent(QResizeEvent* event) override;
  void mouseMoveEvent(QMouseEvent* event) override;
  void mousePressEvent(QMouseEvent* event) override;
  void mouseReleaseEvent(QMouseEvent* event) override;
  void wheelEvent(QWheelEvent* event) override;
  void keyPressEvent(QKeyEvent* event) override;
  void keyReleaseEvent(QKeyEvent* event) override;

 private:
  DomainVisualizer* domain_visualizer_;

  QVulkanInstance q_vulkan_instance_;

  DomainVisualizerVulkanWindowRenderer* renderer_ = nullptr;
};

std::unique_ptr<QWidget> MakeDomainVisualizerWidget(
    DomainVisualizer* domain_visualizer, QWidget* parent);

}  // namespace ide
}  // namespace typescript_cpp_v8
}  // namespace reify

#endif  // _SRC_IDT_TARGETS_TYPESCRIPT_CPP_V8_IDE_DOMAIN_VISUALIZER_QT_WIDGET_H
