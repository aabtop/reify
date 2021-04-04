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
       static_cast<uint32_t>(image_size.height())},
      get_view_matrix_());
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

DomainVisualizerVulkanWindow::DomainVisualizerVulkanWindow()
    : free_camera_viewport_(0, 0) {
  QSize viewport_size = swapChainImageSize();
  free_camera_viewport_.AccumulateViewportResize(viewport_size.width(),
                                                 viewport_size.height());
}

QVulkanWindowRenderer* DomainVisualizerVulkanWindow::createRenderer() {
  renderer_ = new DomainVisualizerVulkanWindowRenderer(
      this, [&free_camera_viewport = free_camera_viewport_,
             &last_tick_time = last_tick_time_]() {
        auto current_time = std::chrono::high_resolution_clock::now();
        if (last_tick_time) {
          free_camera_viewport.AccumulateTimeDelta(current_time -
                                                   *last_tick_time);
        }
        last_tick_time = current_time;
        return free_camera_viewport.ViewMatrix();
      });
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

void DomainVisualizerVulkanWindow::resizeEvent(QResizeEvent* event) {
  free_camera_viewport_.AccumulateViewportResize(event->size().width(),
                                                 event->size().height());
}

void DomainVisualizerVulkanWindow::mouseMoveEvent(QMouseEvent* event) {
  free_camera_viewport_.AccumulateMouseMove(event->x(), event->y());
}

namespace {
FreeCameraViewport3d::MouseButton ConvertMouseButtonFromQt(
    Qt::MouseButton qt_mouse_button) {
  switch (qt_mouse_button) {
    case Qt::LeftButton:
      return FreeCameraViewport3d::MouseButton::Left;
    case Qt::RightButton:
      return FreeCameraViewport3d::MouseButton::Right;
    default:
      return FreeCameraViewport3d::MouseButton::Unknown;
  }
}
}  // namespace

void DomainVisualizerVulkanWindow::mousePressEvent(QMouseEvent* event) {
  free_camera_viewport_.AccumulateMouseButtonEvent(
      ConvertMouseButtonFromQt(event->button()), true, event->x(), event->y());
}
void DomainVisualizerVulkanWindow::mouseReleaseEvent(QMouseEvent* event) {
  free_camera_viewport_.AccumulateMouseButtonEvent(
      ConvertMouseButtonFromQt(event->button()), false, event->x(), event->y());
}

void DomainVisualizerVulkanWindow::wheelEvent(QWheelEvent* event) {
  free_camera_viewport_.AccumulateMouseWheelEvent(event->angleDelta().y() /
                                                  8.0f);
}

namespace {
int ConvertKeyFromQt(int key) {
  // Since the keycode accepted by FreeCameraViewport3d is defined by the Qt
  // key mapping, the conversion is the identity.
  return key;
}
}  // namespace
void DomainVisualizerVulkanWindow::keyPressEvent(QKeyEvent* event) {
  free_camera_viewport_.AccumulateKeyboardEvent(ConvertKeyFromQt(event->key()),
                                                true);
}
void DomainVisualizerVulkanWindow::keyReleaseEvent(QKeyEvent* event) {
  free_camera_viewport_.AccumulateKeyboardEvent(ConvertKeyFromQt(event->key()),
                                                false);
}
