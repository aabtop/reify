#ifndef _IDT_TARGETS_TYPESCRIPT_CPP_V8_SYMBOL_VISUALIZER_IMGUI_WIDGETS_H_
#define _IDT_TARGETS_TYPESCRIPT_CPP_V8_SYMBOL_VISUALIZER_IMGUI_WIDGETS_H_

#include "imgui.h"

namespace reify {
namespace typescript_cpp_v8 {
namespace imgui {

void Spinner(const char* label, const float indicator_radius,
             const ImVec4& main_color, const ImVec4& backdrop_color,
             const int circle_count, const float speed);

}  // namespace imgui
}  // namespace typescript_cpp_v8
}  // namespace reify

#endif  // _IDT_TARGETS_TYPESCRIPT_CPP_V8_SYMBOL_VISUALIZER_IMGUI_WIDGETS_H_
