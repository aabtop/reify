#include "reify/window/window_stack.h"

#include "reify/utils/error.h"
#include "vulkan_utils/vulkan_utils.h"

namespace reify {
namespace window {

WindowStack::WindowStack(const std::vector<Window*>& sub_windows)
    : sub_windows_(sub_windows) {}

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

  utils::ErrorOr<FrameResources> RenderFrame(
      VkCommandBuffer command_buffer, VkFramebuffer framebuffer,
      VkImage output_color_image, const Rect& viewport_region) override {
    vulkan_utils::SetImageLayout(
        command_buffer, output_color_image, VK_IMAGE_ASPECT_COLOR_BIT,
        VK_IMAGE_LAYOUT_UNDEFINED, VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL);

    std::vector<FrameResources> frame_resources;
    for (const auto& sub_renderer : sub_renderers_) {
      REIFY_UTILS_ASSIGN_OR_RETURN(
          sub_frame_resources,
          sub_renderer->RenderFrame(command_buffer, framebuffer,
                                    output_color_image, viewport_region));
      frame_resources.push_back(std::move(sub_frame_resources));
    }

    vulkan_utils::SetImageLayout(command_buffer, output_color_image,
                                 VK_IMAGE_ASPECT_COLOR_BIT,
                                 VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL,
                                 VK_IMAGE_LAYOUT_PRESENT_SRC_KHR);

    return std::move(frame_resources);
  }

 private:
  std::vector<std::unique_ptr<Window::Renderer>> sub_renderers_;
};

}  // namespace

utils::ErrorOr<std::unique_ptr<Window::Renderer>> WindowStack::CreateRenderer(
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
