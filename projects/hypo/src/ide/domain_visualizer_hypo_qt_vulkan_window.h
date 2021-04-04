#ifndef _IDE_DOMAIN_VISUALIZER_HYPO_QT_VULKAN_WINDOW_H
#define _IDE_DOMAIN_VISUALIZER_HYPO_QT_VULKAN_WINDOW_H

#include <QKeyEvent>
#include <QMouseEvent>
#include <QResizeEvent>
#include <QVulkanInstance>
#include <QVulkanWindow>
#include <optional>

#include "src/ide/free_camera_viewport_3d.h"
#include "src/ide/vulkan/renderer.h"

class DomainVisualizerVulkanWindowRenderer : public QVulkanWindowRenderer {
 public:
  DomainVisualizerVulkanWindowRenderer(
      QVulkanWindow* window, const std::function<glm::mat4()>& get_view_matrix)
      : window_(window), get_view_matrix_(get_view_matrix) {}

  void initResources() override;

  void initSwapChainResources() override{};
  void releaseSwapChainResources() override{};
  void releaseResources() override;

  void startNextFrame() override;

  void SetTriangleSoup(std::shared_ptr<const TriangleSoup> triangle_soup);

 private:
  void SetTriangleSoupOnRenderer(
      std::shared_ptr<const TriangleSoup> triangle_soup);

  QVulkanWindow* window_;
  std::optional<Renderer> renderer_;

  std::vector<std::optional<Renderer::FrameResources>> frame_resources_;

  std::shared_ptr<const TriangleSoup> pending_triangle_soup_;
  std::function<glm::mat4()> get_view_matrix_;
};

class DomainVisualizerVulkanWindow : public QVulkanWindow {
  Q_OBJECT

 public:
  DomainVisualizerVulkanWindow();
  QVulkanWindowRenderer* createRenderer() override;

 public slots:
  void SetTriangleSoup(std::shared_ptr<const TriangleSoup> triangle_soup);

 protected:
  void resizeEvent(QResizeEvent* event) override;
  void mouseMoveEvent(QMouseEvent* event) override;
  void mousePressEvent(QMouseEvent* event) override;
  void mouseReleaseEvent(QMouseEvent* event) override;
  void wheelEvent(QWheelEvent* event) override;
  void keyPressEvent(QKeyEvent* event) override;
  void keyReleaseEvent(QKeyEvent* event) override;

 private:
  DomainVisualizerVulkanWindowRenderer* renderer_ = nullptr;

  // In case a triangle soup becomes available before our renderer is created.
  std::shared_ptr<const TriangleSoup> pending_triangle_soup_;

  std::optional<std::chrono::time_point<std::chrono::high_resolution_clock>>
      last_tick_time_;
  FreeCameraViewport3d free_camera_viewport_;
};

#endif  // _IDE_DOMAIN_VISUALIZER_HYPO_QT_VULKAN_WINDOW_H
