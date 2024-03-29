#include "reify/window/window_viewport.h"

#include "reify/utils/error.h"

namespace reify {
namespace window {

WindowViewport::WindowViewport(Window* sub_window) : sub_window_(sub_window) {}

bool WindowViewport::OnInputEvent(const InputEvent& input_event) {
  if (!viewport_) {
    return false;
  }

  auto viewport_modified_event = [viewport = *viewport_](auto event) {
    decltype(event) modified_event = event;
    modified_event.x -= viewport.left;
    modified_event.y -= viewport.top;
    return modified_event;
  };

  if (auto event = std::get_if<MouseMoveEvent>(&input_event)) {
    return sub_window_->OnInputEvent(viewport_modified_event(*event));
  } else if (auto event = std::get_if<MouseButtonEvent>(&input_event)) {
    if (viewport_->inside(event->x, event->y) || !event->pressed) {
      return sub_window_->OnInputEvent(viewport_modified_event(*event));
    } else {
      return false;
    }
  } else if (auto event = std::get_if<MouseWheelEvent>(&input_event)) {
    return sub_window_->OnInputEvent(viewport_modified_event(*event));
  } else {
    return sub_window_->OnInputEvent(input_event);
  }
}

void WindowViewport::OnViewportResize(const std::array<int, 2>& size) {
  // This value is saved and taken to mean the size of the parent window, and
  // ultimately is used to size the renderer frame.
  on_viewport_resize_size_ = size;
}

void WindowViewport::AdvanceTime(std::chrono::duration<float> seconds) {
  sub_window_->AdvanceTime(seconds);
}

void WindowViewport::SetViewport(const Rect& viewport) {
  if (!viewport_ || viewport_->width() != viewport.width() ||
      viewport_->height() != viewport.height()) {
    sub_window_->OnViewportResize({viewport.width(), viewport.height()});
  }
  viewport_ = viewport;
}

namespace {

class RendererWindowViewport : public Window::Renderer {
 public:
  RendererWindowViewport(std::unique_ptr<Renderer>&& sub_renderer,
                         const WindowViewport* window_viewport)
      : sub_renderer_(std::move(sub_renderer)),
        window_viewport_(window_viewport) {}

  utils::ErrorOr<FrameResources> RenderFrame(
      VkCommandBuffer command_buffer, VkFramebuffer framebuffer,
      VkImage output_color_image, const Rect& viewport_region) override {
    if (!window_viewport_->viewport() ||
        !window_viewport_->get_on_viewport_resize_size()) {
      return FrameResources();
    }

    // Scale the window viewport by the parent view's size.
    std::array<float, 2> scale = {
        viewport_region.width() /
            static_cast<float>((*window_viewport_->get_on_viewport_resize_size())[0]),
        viewport_region.height() /
            static_cast<float>((*window_viewport_->get_on_viewport_resize_size())[1]),
    };
    Rect scaled_viewport = *window_viewport_->viewport();
    scaled_viewport.left *= scale[0];
    scaled_viewport.top *= scale[1];
    scaled_viewport.right *= scale[0];
    scaled_viewport.bottom *= scale[1];
    return sub_renderer_->RenderFrame(
        command_buffer, framebuffer, output_color_image,
        Rect::Intersect(scaled_viewport, viewport_region));
  }

 private:
  std::unique_ptr<Window::Renderer> sub_renderer_;
  const WindowViewport* window_viewport_;
};

}  // namespace

utils::ErrorOr<std::unique_ptr<Window::Renderer>>
WindowViewport::CreateRenderer(VkInstance instance,
                               VkPhysicalDevice physical_device,
                               VkDevice device, VkFormat output_image_format) {
  REIFY_UTILS_ASSIGN_OR_RETURN(
      sub_renderer, sub_window_->CreateRenderer(instance, physical_device,
                                                device, output_image_format));
  return std::unique_ptr<Renderer>(
      new RendererWindowViewport(std::move(sub_renderer), this));
}

}  // namespace window
}  // namespace reify
