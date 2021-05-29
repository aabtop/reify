#ifndef _IDT_TARGETS_TYPESCRIPT_CPP_V8_DOMAIN_VISUALIZER_IMGUI_STATUS_LAYER_H
#define _IDT_TARGETS_TYPESCRIPT_CPP_V8_DOMAIN_VISUALIZER_IMGUI_STATUS_LAYER_H

#include <functional>
#include <vector>

namespace reify {
namespace typescript_cpp_v8 {
namespace imgui {

class StatusLayer {
 public:
  StatusLayer();

  void ExecuteImGuiCommands();

  class Window {
   public:
    Window(StatusLayer* layer, const std::function<void()>& draw_function)
        : layer_(layer), draw_function_(draw_function) {
      layer_->Register(this);
    }
    ~Window() { layer_->Unregister(this); }

    const std::function<void()>& draw_function() { return draw_function_; }

   private:
    StatusLayer* layer_;
    std::function<void()> draw_function_;
  };

 private:
  friend class Window;
  void Register(Window* window);
  void Unregister(Window* window);

  std::vector<Window*> windows_;
};

}  // namespace imgui
}  // namespace typescript_cpp_v8
}  // namespace reify

#endif  // _IDT_TARGETS_TYPESCRIPT_CPP_V8_DOMAIN_VISUALIZER_IMGUI_STATUS_LAYER_H
