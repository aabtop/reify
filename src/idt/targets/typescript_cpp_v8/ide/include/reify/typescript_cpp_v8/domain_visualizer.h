#ifndef _IDE_DOMAIN_VISUALIZER_H
#define _IDE_DOMAIN_VISUALIZER_H

#include <vulkan/vulkan.h>

#include <any>
#include <array>
#include <functional>
#include <optional>
#include <string>
#include <vector>

#include "reify/typescript_cpp_v8.h"

namespace reify {
namespace typescript_cpp_v8 {

class DomainVisualizer {
 public:
  struct Error {
    std::string msg;
  };
  template <typename T>
  using ErrorOr = std::variant<Error, T>;

  struct MouseMoveEvent {
    int x;
    int y;
  };
  enum class MouseButton {
    Left,
    Right,
    Unknown,
    Count = Unknown,
  };
  struct MouseButtonEvent {
    MouseButton button;
    bool pressed;
    int x;
    int y;
  };
  struct MouseWheelEvent {
    float angle_in_degrees;
  };
  struct KeyboardEvent {
    // Keycode is defined by Qt's key mapping:
    // https://doc.qt.io/qt-5/qt.html#Key-enum
    int key;
    bool pressed;
  };
  using InputEvent = std::variant<MouseMoveEvent, MouseButtonEvent,
                                  MouseWheelEvent, KeyboardEvent>;
  class Renderer {
   public:
    template <typename T>
    using ErrorOr = ErrorOr<T>;
    using FrameResources = std::any;
    virtual ~Renderer() {}
    virtual ErrorOr<FrameResources> RenderFrame(
        VkCommandBuffer command_buffer, VkFramebuffer framebuffer,
        const std::array<uint32_t, 2>& output_surface_size) = 0;
  };

  using PreparedSymbol = std::any;

  virtual ~DomainVisualizer(){};

  virtual std::vector<reify::CompilerEnvironment::InputModule>
  GetTypeScriptModules() = 0;

  virtual bool CanPreviewSymbol(
      const reify::CompiledModule::ExportedSymbol& symbol) = 0;

  // The callback, `on_preview_prepared`, may be called from a separate thread.
  virtual void PrepareSymbolForPreview(
      std::shared_ptr<reify::CompiledModule> module,
      const reify::CompiledModule::ExportedSymbol& symbol,
      const std::function<void(ErrorOr<PreparedSymbol>)>&
          on_preview_prepared) = 0;
  virtual void Preview(const PreparedSymbol& prepared_symbol) = 0;

  virtual void OnInputEvent(const InputEvent& input_event) = 0;

  virtual void OnViewportResize(const std::array<int, 2>& size) = 0;

  virtual void AdvanceTime(std::chrono::duration<float> seconds) = 0;

  virtual ErrorOr<std::unique_ptr<Renderer>> CreateRenderer(
      VkInstance instance, VkPhysicalDevice physical_device, VkDevice device,
      VkFormat output_image_format) = 0;
};

}  // namespace typescript_cpp_v8
}  // namespace reify

#endif  // _IDE_DOMAIN_VISUALIZER_H
