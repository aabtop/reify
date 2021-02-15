#include "src/ide/domain_visualizer_hypo_qt_vulkan_window.h"

void DomainVisualizerVulkanWindowRenderer::initResources() {
  auto renderer_or_error = Renderer::Create(
      window_->vulkanInstance()->vkInstance(), window_->physicalDevice(),
      window_->device(), window_->colorFormat());
  if (auto error = std::get_if<Renderer::Error>(&renderer_or_error)) {
    qFatal("Error creating Vulkan renderer: %s", error->msg.c_str());
  }
  renderer_.emplace(std::move(std::get<Renderer>(renderer_or_error)));

  if (pending_triangle_soup_) {
    SetTriangleSoupOnRenderer(pending_triangle_soup_);
    pending_triangle_soup_ = nullptr;
  }
}

void DomainVisualizerVulkanWindowRenderer::releaseResources() {
  frame_resources_.clear();
  renderer_ = std::nullopt;
};

void DomainVisualizerVulkanWindowRenderer::startNextFrame() {
  int current_frame_index = window_->currentFrame();

  if (current_frame_index >= frame_resources_.size()) {
    frame_resources_.resize(current_frame_index + 1);
  }

  // Clean up previous frame's resources.
  frame_resources_[current_frame_index] = std::nullopt;

  QSize image_size = window_->swapChainImageSize();
  auto error_or_frame_resources = renderer_->RenderFrame(
      window_->currentCommandBuffer(), window_->currentFramebuffer(),
      {static_cast<uint32_t>(image_size.width()),
       static_cast<uint32_t>(image_size.height())});
  if (auto error = std::get_if<Renderer::Error>(&error_or_frame_resources)) {
    qFatal("Vulkan error while rendering frame: %s", error->msg.c_str());
  }

  frame_resources_[current_frame_index] =
      std::move(std::get<Renderer::FrameResources>(error_or_frame_resources));

  window_->frameReady();
  // render continuously, throttled by the presentation rate.
  window_->requestUpdate();
};

void DomainVisualizerVulkanWindowRenderer::SetTriangleSoup(
    std::shared_ptr<const TriangleSoup> triangle_soup) {
  if (renderer_) {
    SetTriangleSoupOnRenderer(triangle_soup);
  } else {
    pending_triangle_soup_ = triangle_soup;
  }
}

void DomainVisualizerVulkanWindowRenderer::SetTriangleSoupOnRenderer(
    std::shared_ptr<const TriangleSoup> triangle_soup) {
  auto error = renderer_->SetTriangleSoup(triangle_soup);
  if (error) {
    qFatal("Vulkan error while creating buffers for new mesh: %s",
           error->msg.c_str());
  }
}

QVulkanWindowRenderer* DomainVisualizerVulkanWindow::createRenderer() {
  renderer_ = new DomainVisualizerVulkanWindowRenderer(this);
  if (pending_triangle_soup_) {
    renderer_->SetTriangleSoup(pending_triangle_soup_);
    pending_triangle_soup_ = nullptr;
  }
  return renderer_;
}

void DomainVisualizerVulkanWindow::SetTriangleSoup(
    const std::shared_ptr<const TriangleSoup> triangle_soup) {
  if (renderer_) {
    renderer_->SetTriangleSoup(triangle_soup);
  } else {
    pending_triangle_soup_ = triangle_soup;
  }
}
