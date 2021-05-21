#ifndef _SRC_IDT_TARGETS_TYPESCRIPT_CPP_V8_IDE_DOMAIN_VISUALIZER_QT_WIDGET_H
#define _SRC_IDT_TARGETS_TYPESCRIPT_CPP_V8_IDE_DOMAIN_VISUALIZER_QT_WIDGET_H

#include <QVulkanInstance>
#include <QVulkanWindow>
#include <QWidget>
#include <memory>

#include "reify/window/window.h"

namespace reify {
namespace typescript_cpp_v8 {
namespace ide {

class DomainVisualizerVulkanWindowRenderer : public QVulkanWindowRenderer {
 public:
  DomainVisualizerVulkanWindowRenderer(QVulkanWindow* window,
                                       window::Window* reify_window)
      : reify_window_(reify_window), window_(window) {}

  void initResources() override;

  void initSwapChainResources() override{};
  void releaseSwapChainResources() override{};
  void releaseResources() override;

  void startNextFrame() override;

 private:
  window::Window* reify_window_;

  QVulkanWindow* window_;
  std::unique_ptr<window::Window::Renderer> reify_window_renderer_;

  std::vector<std::optional<window::Window::Renderer::FrameResources>>
      frame_resources_;

  std::optional<std::chrono::time_point<std::chrono::high_resolution_clock>>
      last_render_time_;
};

class DomainVisualizerVulkanWindow : public QVulkanWindow {
  Q_OBJECT

 public:
  DomainVisualizerVulkanWindow(window::Window* reify_window);
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
  window::Window* reify_window_;

  QVulkanInstance q_vulkan_instance_;

  DomainVisualizerVulkanWindowRenderer* renderer_ = nullptr;
};

std::unique_ptr<QWidget> MakeDomainVisualizerWidget(
    window::Window* reify_window, QWidget* parent);

}  // namespace ide
}  // namespace typescript_cpp_v8
}  // namespace reify

#endif  // _SRC_IDT_TARGETS_TYPESCRIPT_CPP_V8_IDE_DOMAIN_VISUALIZER_QT_WIDGET_H
