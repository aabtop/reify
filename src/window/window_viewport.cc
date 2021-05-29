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
    if (viewport_->inside(event->x, event->y)) {
      return sub_window_->OnInputEvent(viewport_modified_event(*event));
    } else {
      return false;
    }
  } else if (auto event = std::get_if<MouseButtonEvent>(&input_event)) {
    if (viewport_->inside(event->x, event->y)) {
      return sub_window_->OnInputEvent(viewport_modified_event(*event));
    } else {
      return false;
    }
  } else {
    return sub_window_->OnInputEvent(input_event);
  }
}

void WindowViewport::OnViewportResize(const std::array<int, 2>& size) {
  if (viewport_) {
    if (viewport_->width() != size[0] || viewport_->height() != size[1]) {
      if (viewport_->width() != size[0]) {
        float change_ratio = size[0] / viewport_->width();
        viewport_->left *= change_ratio;
        viewport_->right = viewport_->left + size[0];
      }
      if (viewport_->height() != size[1]) {
        float change_ratio = size[1] / viewport_->height();
        viewport_->top *= change_ratio;
        viewport_->bottom = viewport_->top + size[1];
      }

      sub_window_->OnViewportResize(size);
    }
  }
}

void WindowViewport::AdvanceTime(std::chrono::duration<float> seconds) {
  sub_window_->AdvanceTime(seconds);
}

void WindowViewport::SetViewport(const Rect& viewport) {
  if (!viewport_ || viewport_->width() != viewport.width() ||
      viewport_->height() != viewport.height()) {
    viewport_ = viewport;
    sub_window_->OnViewportResize({viewport.width(), viewport.height()});
  }
}

namespace {

class RendererWindowViewport : public Window::Renderer {
 public:
  RendererWindowViewport(std::unique_ptr<Renderer>&& sub_renderer,
                         const WindowViewport* window_viewport)
      : sub_renderer_(std::move(sub_renderer)),
        window_viewport_(window_viewport) {}

  ErrorOr<FrameResources> RenderFrame(VkCommandBuffer command_buffer,
                                      VkFramebuffer framebuffer,
                                      const Rect& viewport_region) override {
    if (!window_viewport_->viewport()) {
      return FrameResources();
    }

    return sub_renderer_->RenderFrame(command_buffer, framebuffer,
                                      *window_viewport_->viewport());
  }

 private:
  std::unique_ptr<Window::Renderer> sub_renderer_;
  const WindowViewport* window_viewport_;
};

}  // namespace

Window::ErrorOr<std::unique_ptr<Window::Renderer>>
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
