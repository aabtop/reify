#ifndef _IDT_TARGETS_TYPESCRIPT_CPP_V8_SYMBOL_VISUALIZER_IMGUI_UTILS_H_
#define _IDT_TARGETS_TYPESCRIPT_CPP_V8_SYMBOL_VISUALIZER_IMGUI_UTILS_H_

#include "imgui.h"
#include "imgui_internal.h"

namespace reify {
namespace typescript_cpp_v8 {
namespace imgui {

class DisableIf {
 public:
  DisableIf(bool condition) : condition_(condition) {
    if (condition_) {
      ImGui::PushItemFlag(ImGuiItemFlags_Disabled, true);
      ImGui::PushStyleVar(ImGuiStyleVar_Alpha, ImGui::GetStyle().Alpha * 0.5f);
    }
  }
  ~DisableIf() {
    if (condition_) {
      ImGui::PopItemFlag();
      ImGui::PopStyleVar();
    }
  }

  bool condition() const { return condition_; }

 private:
  bool condition_;
};

}  // namespace imgui
}  // namespace typescript_cpp_v8
}  // namespace reify

#endif  // _IDT_TARGETS_TYPESCRIPT_CPP_V8_SYMBOL_VISUALIZER_IMGUI_UTILS_H_
