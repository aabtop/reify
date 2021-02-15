#ifndef _IDE_DOMAIN_VISUALIZER_HYPO_QT_VULKAN_WINDOW_H
#define _IDE_DOMAIN_VISUALIZER_HYPO_QT_VULKAN_WINDOW_H

#include <QVulkanInstance>
#include <QVulkanWindow>
#include <optional>

#include "src/ide/vulkan/renderer.h"

class DomainVisualizerVulkanWindowRenderer : public QVulkanWindowRenderer {
 public:
  DomainVisualizerVulkanWindowRenderer(QVulkanWindow* window)
      : window_(window) {}

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
};

class DomainVisualizerVulkanWindow : public QVulkanWindow {
  Q_OBJECT

 public:
  QVulkanWindowRenderer* createRenderer() override;

 public slots:
  void SetTriangleSoup(std::shared_ptr<const TriangleSoup> triangle_soup);

 private:
  DomainVisualizerVulkanWindowRenderer* renderer_ = nullptr;

  // In case a triangle soup becomes available before our renderer is created.
  std::shared_ptr<const TriangleSoup> pending_triangle_soup_;
};

#endif  // _IDE_DOMAIN_VISUALIZER_HYPO_QT_VULKAN_WINDOW_H
