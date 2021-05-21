#include "reify/window/window_stack.h"

#include "reify/utils/error.h"

namespace reify {
namespace window {

WindowStack::WindowStack(std::vector<std::unique_ptr<Window>>&& sub_windows)
    : sub_windows_(std::move(sub_windows)) {}

bool WindowStack::OnInputEvent(const InputEvent& input_event) {
  for (auto iter = sub_windows_.rbegin(); iter != sub_windows_.rend(); ++iter) {
    if (!(*iter)->OnInputEvent(input_event)) {
      return false;
    }
  }
  return true;
}

void WindowStack::OnViewportResize(const std::array<int, 2>& size) {
  for (const auto& sub_window : sub_windows_) {
    sub_window->OnViewportResize(size);
  }
}

void WindowStack::AdvanceTime(std::chrono::duration<float> seconds) {
  for (const auto& sub_window : sub_windows_) {
    sub_window->AdvanceTime(seconds);
  }
}

namespace {

class RendererWindowStack : public Window::Renderer {
 public:
  RendererWindowStack(std::vector<std::unique_ptr<Renderer>>&& sub_renderers)
      : sub_renderers_(std::move(sub_renderers)) {}

  ErrorOr<FrameResources> RenderFrame(
      VkCommandBuffer command_buffer, VkFramebuffer framebuffer,
      const std::array<uint32_t, 2>& output_surface_size) override {
    std::vector<FrameResources> frame_resources;
    for (const auto& sub_renderer : sub_renderers_) {
      REIFY_UTILS_ASSIGN_OR_RETURN(
          sub_frame_resources,
          sub_renderer->RenderFrame(command_buffer, framebuffer,
                                    output_surface_size));
      frame_resources.push_back(std::move(sub_frame_resources));
    }
    return std::move(frame_resources);
  }

 private:
  std::vector<std::unique_ptr<Window::Renderer>> sub_renderers_;
};

}  // namespace

Window::ErrorOr<std::unique_ptr<Window::Renderer>> WindowStack::CreateRenderer(
    VkInstance instance, VkPhysicalDevice physical_device, VkDevice device,
    VkFormat output_image_format) {
  std::vector<std::unique_ptr<Window::Renderer>> sub_renderers;
  sub_renderers.reserve(sub_windows_.size());
  for (const auto& sub_window : sub_windows_) {
    REIFY_UTILS_ASSIGN_OR_RETURN(
        sub_renderer, sub_window->CreateRenderer(instance, physical_device,
                                                 device, output_image_format));
    sub_renderers.push_back(std::move(sub_renderer));
  }
  return std::unique_ptr<Renderer>(
      new RendererWindowStack(std::move(sub_renderers)));
}

}  // namespace window
}  // namespace reify
