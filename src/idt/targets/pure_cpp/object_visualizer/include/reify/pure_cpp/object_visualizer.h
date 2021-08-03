#ifndef _REIFY_PURE_CPP_OBJECT_VISUALIZER_H_
#define _REIFY_PURE_CPP_OBJECT_VISUALIZER_H_

#include <any>
#include <optional>
#include <string>

#include "reify/utils/error.h"
#include "reify/utils/future.h"
#include "reify/window/window.h"

namespace reify {
namespace pure_cpp {

class ImGuiVisualizer {
 public:
  virtual std::string ImGuiWindowPanelTitle() const = 0;
  virtual void RenderImGuiWindow() = 0;
};

template <typename T>
class ObjectVisualizer {
 public:
  virtual utils::Future<utils::ErrorOr<std::any>> PrepareDataForPreview(
      const T& data) = 0;
  virtual void SetPreview(const std::optional<std::any>& prepared_symbol) = 0;

  virtual window::Window* GetWindow() const = 0;
  virtual ImGuiVisualizer* GetImGuiVisualizer() const { return nullptr; }
};

}  // namespace pure_cpp
}  // namespace reify

#endif  // _REIFY_PURE_CPP_OBJECT_VISUALIZER_H_
