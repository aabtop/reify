#include "src/idt/targets/typescript_cpp_v8/ide/domain_visualizer_qt_widget.h"

#include <vulkan/vulkan.h>

#include <QCoreApplication>
#include <QKeyEvent>
#include <QMouseEvent>
#include <QResizeEvent>
#include <QVBoxLayout>
#include <QWheelEvent>
#include <filesystem>
#include <iostream>

#include "reify/typescript_cpp_v8/domain_visualizer.h"

namespace reify {
namespace typescript_cpp_v8 {
namespace ide {

namespace {

#if defined(BAZEL_TARGET_OS_LINUX)

// Returns an absolute path to the current executable.
std::filesystem::path GetAbsoluteExecutableDir() {
  return std::filesystem::absolute(
      QCoreApplication::applicationDirPath().toStdString());
}

std::optional<std::filesystem::path> FindSoLibDir() {
  std::optional<std::filesystem::path> previous_path;
  for (std::filesystem::path current_path = GetAbsoluteExecutableDir();
       current_path.has_parent_path() &&
       (!previous_path || *previous_path != current_path);
       current_path = current_path.parent_path()) {
    previous_path = current_path;
    for (const auto& p : std::filesystem::directory_iterator(current_path)) {
      if (!std::filesystem::is_directory(p.path())) {
        continue;
      }

      auto path_as_string = p.path().filename().string();
      if (path_as_string.rfind("_solib", 0) == 0) {
        return p.path();
      }
    }
  }
  return std::nullopt;
}

// Walks up the directory tree back to the root looking for vulkan.so.1 file,
// so that we can use the one that's packaged with this application.
std::optional<std::filesystem::path> FindVulkanSo1() {
  auto so_lib_dir = FindSoLibDir();
  if (!so_lib_dir) {
    return std::nullopt;
  }

  for (const auto& p :
       std::filesystem::recursive_directory_iterator(*so_lib_dir)) {
    if (!std::filesystem::is_regular_file(p.path())) {
      continue;
    }

    if (p.path().filename() == "libvulkan.so.1") {
      return p.path();
    }
  }

  return std::nullopt;
}

#else  // #if defined(BAZEL_TARGET_OS_LINUX)

std::optional<std::filesystem::path> FindVulkanSo1() { return std::nullopt; }

#endif  // #if defined(BAZEL_TARGET_OS_LINUX)

}  // namespace

void DomainVisualizerVulkanWindowRenderer::initResources() {
  auto renderer_or_error = domain_visualizer_->CreateRenderer(
      window_->vulkanInstance()->vkInstance(), window_->physicalDevice(),
      window_->device(), window_->colorFormat());
  if (auto error = std::get_if<0>(&renderer_or_error)) {
    qFatal("Error creating Vulkan renderer: %s", error->msg.c_str());
  }
  domain_visualizer_renderer_ = std::move(std::get<1>(renderer_or_error));
}

void DomainVisualizerVulkanWindowRenderer::releaseResources() {
  frame_resources_.clear();
  domain_visualizer_renderer_.reset();
};

void DomainVisualizerVulkanWindowRenderer::startNextFrame() {
  int current_frame_index = window_->currentFrame();

  if (current_frame_index >= frame_resources_.size()) {
    frame_resources_.resize(current_frame_index + 1);
  }

  // Clean up previous frame's resources.
  frame_resources_[current_frame_index] = std::nullopt;

  // Update the clock on the domain visualizer.
  auto current_time = std::chrono::high_resolution_clock::now();
  if (last_render_time_) {
    domain_visualizer_->AdvanceTime(current_time - *last_render_time_);
  }
  last_render_time_ = current_time;

  QSize image_size = window_->swapChainImageSize();
  auto error_or_frame_resources = domain_visualizer_renderer_->RenderFrame(
      window_->currentCommandBuffer(), window_->currentFramebuffer(),
      {static_cast<uint32_t>(image_size.width()),
       static_cast<uint32_t>(image_size.height())});
  if (auto error =
          std::get_if<DomainVisualizer::Error>(&error_or_frame_resources)) {
    qFatal("Vulkan error while rendering frame: %s", error->msg.c_str());
  }

  frame_resources_[current_frame_index] =
      std::move(std::get<DomainVisualizer::Renderer::FrameResources>(
          error_or_frame_resources));

  window_->frameReady();
  // render continuously, throttled by the presentation rate.
  window_->requestUpdate();
};

DomainVisualizerVulkanWindow::DomainVisualizerVulkanWindow(
    DomainVisualizer* domain_visualizer)
    : domain_visualizer_(domain_visualizer) {
  auto maybe_vulkan_so_1_path = FindVulkanSo1();
  if (maybe_vulkan_so_1_path) {
    // Qt doesn't know where our custom built Vulkan loader is on Linux, so
    // we have to find it and specify it manually.
    qputenv("QT_VULKAN_LIB", QByteArray(reinterpret_cast<const char*>(
                                 maybe_vulkan_so_1_path->c_str())));
  } else {
    std::cerr << "Could not find local build of vulkan.so.1. "
              << "Maybe the system version will work..." << std::endl;
  }

  q_vulkan_instance_.setLayers(QByteArrayList()
                               << "VK_LAYER_LUNARG_standard_validation");
  if (!q_vulkan_instance_.create())
    qFatal("Failed to create Vulkan instance: %d",
           q_vulkan_instance_.errorCode());

  this->setVulkanInstance(&q_vulkan_instance_);

  QSize viewport_size = swapChainImageSize();
  domain_visualizer_->OnViewportResize(
      {viewport_size.width(), viewport_size.height()});
}

QVulkanWindowRenderer* DomainVisualizerVulkanWindow::createRenderer() {
  renderer_ =
      new DomainVisualizerVulkanWindowRenderer(this, domain_visualizer_);

  return renderer_;
}

void DomainVisualizerVulkanWindow::resizeEvent(QResizeEvent* event) {
  domain_visualizer_->OnViewportResize(
      {event->size().width(), event->size().height()});
}

void DomainVisualizerVulkanWindow::mouseMoveEvent(QMouseEvent* event) {
  domain_visualizer_->OnInputEvent(
      DomainVisualizer::MouseMoveEvent{event->x(), event->y()});
}

namespace {
DomainVisualizer::MouseButton ConvertMouseButtonFromQt(
    Qt::MouseButton qt_mouse_button) {
  switch (qt_mouse_button) {
    case Qt::LeftButton:
      return DomainVisualizer::MouseButton::Left;
    case Qt::RightButton:
      return DomainVisualizer::MouseButton::Right;
    default:
      return DomainVisualizer::MouseButton::Unknown;
  }
}
}  // namespace

void DomainVisualizerVulkanWindow::mousePressEvent(QMouseEvent* event) {
  domain_visualizer_->OnInputEvent(DomainVisualizer::MouseButtonEvent{
      ConvertMouseButtonFromQt(event->button()), true, event->x(), event->y()});
}
void DomainVisualizerVulkanWindow::mouseReleaseEvent(QMouseEvent* event) {
  domain_visualizer_->OnInputEvent(DomainVisualizer::MouseButtonEvent{
      ConvertMouseButtonFromQt(event->button()), false, event->x(),
      event->y()});
}

void DomainVisualizerVulkanWindow::wheelEvent(QWheelEvent* event) {
  domain_visualizer_->OnInputEvent(
      DomainVisualizer::MouseWheelEvent{event->angleDelta().y() / 8.0f});
}

namespace {
int ConvertKeyFromQt(int key) {
  // Since the keycode accepted by DomainVisualizer is defined by the Qt
  // key mapping, the conversion is the identity.
  return key;
}
}  // namespace
void DomainVisualizerVulkanWindow::keyPressEvent(QKeyEvent* event) {
  domain_visualizer_->OnInputEvent(
      DomainVisualizer::KeyboardEvent{ConvertKeyFromQt(event->key()), true});
}
void DomainVisualizerVulkanWindow::keyReleaseEvent(QKeyEvent* event) {
  domain_visualizer_->OnInputEvent(
      DomainVisualizer::KeyboardEvent{ConvertKeyFromQt(event->key()), false});
}

std::unique_ptr<QWidget> MakeDomainVisualizerWidget(
    DomainVisualizer* domain_visualizer, QWidget* parent) {
  auto vulkan_window = new DomainVisualizerVulkanWindow(domain_visualizer);

  std::unique_ptr<QWidget> domain_visualizer_widget(
      QWidget::createWindowContainer(vulkan_window, parent));

  QVBoxLayout* layout = new QVBoxLayout();
  layout->addWidget(domain_visualizer_widget.get());
  parent->setLayout(layout);

  return domain_visualizer_widget;
}

}  // namespace ide
}  // namespace typescript_cpp_v8
}  // namespace reify