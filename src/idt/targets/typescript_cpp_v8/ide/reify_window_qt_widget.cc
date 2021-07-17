#include "src/idt/targets/typescript_cpp_v8/ide/reify_window_qt_widget.h"

#include <vulkan/vulkan.h>

#include <QCoreApplication>
#include <QKeyEvent>
#include <QMouseEvent>
#include <QResizeEvent>
#include <QVBoxLayout>
#include <QWheelEvent>
#include <filesystem>
#include <iostream>

#include "reify/window/window.h"

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

void ReifyWindowVulkanWindowRenderer::initResources() {
  auto renderer_or_error = reify_window_->CreateRenderer(
      window_->vulkanInstance()->vkInstance(), window_->physicalDevice(),
      window_->device(), window_->colorFormat());
  if (auto error = std::get_if<0>(&renderer_or_error)) {
    qFatal("Error creating Vulkan renderer: %s", error->msg.c_str());
  }
  reify_window_renderer_ = std::move(std::get<1>(renderer_or_error));
}

void ReifyWindowVulkanWindowRenderer::releaseResources() {
  frame_resources_.clear();
  reify_window_renderer_.reset();
};

void ReifyWindowVulkanWindowRenderer::startNextFrame() {
  int current_frame_index = window_->currentFrame();

  if (current_frame_index >= frame_resources_.size()) {
    frame_resources_.resize(current_frame_index + 1);
  }

  // Clean up previous frame's resources.
  frame_resources_[current_frame_index] = std::nullopt;

  // Update the clock on the window.
  auto current_time = std::chrono::high_resolution_clock::now();
  if (last_render_time_) {
    reify_window_->AdvanceTime(current_time - *last_render_time_);
  }
  last_render_time_ = current_time;

  QSize image_size = window_->swapChainImageSize();
  auto error_or_frame_resources = reify_window_renderer_->RenderFrame(
      window_->currentCommandBuffer(), window_->currentFramebuffer(),
      window_->swapChainImage(window_->currentSwapChainImageIndex()),
      {0, 0, image_size.width(), image_size.height()});
  if (auto error = std::get_if<utils::Error>(&error_or_frame_resources)) {
    qFatal("Vulkan error while rendering frame: %s", error->msg.c_str());
  }

  frame_resources_[current_frame_index] =
      std::move(std::get<window::Window::Renderer::FrameResources>(
          error_or_frame_resources));

  window_->frameReady();
  // render continuously, throttled by the presentation rate.
  window_->requestUpdate();
};

ReifyWindowVulkanWindow::ReifyWindowVulkanWindow(window::Window* reify_window)
    : reify_window_(reify_window) {
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
  reify_window_->OnViewportResize(
      {viewport_size.width(), viewport_size.height()});
}

QVulkanWindowRenderer* ReifyWindowVulkanWindow::createRenderer() {
  renderer_ = new ReifyWindowVulkanWindowRenderer(this, reify_window_);

  return renderer_;
}

void ReifyWindowVulkanWindow::resizeEvent(QResizeEvent* event) {
  reify_window_->OnViewportResize(
      {event->size().width(), event->size().height()});
}

void ReifyWindowVulkanWindow::mouseMoveEvent(QMouseEvent* event) {
  reify_window_->OnInputEvent(
      window::Window::MouseMoveEvent{event->x(), event->y()});
}

namespace {
window::Window::MouseButton ConvertMouseButtonFromQt(
    Qt::MouseButton qt_mouse_button) {
  switch (qt_mouse_button) {
    case Qt::LeftButton:
      return window::Window::MouseButton::Left;
    case Qt::RightButton:
      return window::Window::MouseButton::Right;
    default:
      return window::Window::MouseButton::Unknown;
  }
}
}  // namespace

void ReifyWindowVulkanWindow::mousePressEvent(QMouseEvent* event) {
  reify_window_->OnInputEvent(window::Window::MouseButtonEvent{
      ConvertMouseButtonFromQt(event->button()), true, event->x(), event->y()});
}
void ReifyWindowVulkanWindow::mouseReleaseEvent(QMouseEvent* event) {
  reify_window_->OnInputEvent(window::Window::MouseButtonEvent{
      ConvertMouseButtonFromQt(event->button()), false, event->x(),
      event->y()});
}

void ReifyWindowVulkanWindow::wheelEvent(QWheelEvent* event) {
  reify_window_->OnInputEvent(
      window::Window::MouseWheelEvent{event->angleDelta().y() / 8.0f});
}

namespace {
int ConvertKeyFromQt(int key) {
  // Since the keycode accepted by window::Window is defined by the Qt
  // key mapping, the conversion is the identity.
  return key;
}
}  // namespace
void ReifyWindowVulkanWindow::keyPressEvent(QKeyEvent* event) {
  reify_window_->OnInputEvent(
      window::Window::KeyboardEvent{ConvertKeyFromQt(event->key()), true});
}
void ReifyWindowVulkanWindow::keyReleaseEvent(QKeyEvent* event) {
  reify_window_->OnInputEvent(
      window::Window::KeyboardEvent{ConvertKeyFromQt(event->key()), false});
}

std::unique_ptr<QWidget> MakeReifyWindowWidget(window::Window* reify_window,
                                               QWidget* parent) {
  auto vulkan_window = new ReifyWindowVulkanWindow(reify_window);

  std::unique_ptr<QWidget> reify_window_widget(
      QWidget::createWindowContainer(vulkan_window, parent));

  QVBoxLayout* layout = new QVBoxLayout();
  layout->addWidget(reify_window_widget.get());
  parent->setLayout(layout);

  return reify_window_widget;
}

}  // namespace ide
}  // namespace typescript_cpp_v8
}  // namespace reify