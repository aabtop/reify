#include "reify/typescript_cpp_v8/imgui/widgets.h"

#include <fmt/format.h>

#include <algorithm>
#include <cmath>

#include "imgui_internal.h"

using namespace ImGui;

namespace reify {
namespace typescript_cpp_v8 {
namespace imgui {

// Code copied over from
// https://github.com/ocornut/imgui/issues/1901#issuecomment-444929973
// Thanks alexsr!
void Spinner(const char* label, const float indicator_radius,
             const ImVec4& main_color, const ImVec4& backdrop_color,
             const int circle_count, const float speed) {
  ImGuiWindow* window = GetCurrentWindow();
  if (window->SkipItems) {
    return;
  }

  ImGuiContext& g = *GImGui;
  const ImGuiID id = window->GetID(label);

  const ImVec2 pos = window->DC.CursorPos;
  const float small_circle_radius = indicator_radius / 10.0f;
  const float big_circle_radius = small_circle_radius * 2;
  const float bb_radius = indicator_radius + big_circle_radius;
  const ImRect bb(pos,
                  ImVec2(pos.x + bb_radius * 2.0f, pos.y + bb_radius * 2.0f));
  const ImGuiStyle& style = g.Style;
  ItemSize(bb, style.FramePadding.y);
  if (!ItemAdd(bb, id)) {
    return;
  }
  const float t = g.Time;
  const auto degree_offset = 2.0f * IM_PI / circle_count;
  for (int i = 0; i < circle_count; ++i) {
    const auto x = indicator_radius * std::sin(degree_offset * i);
    const auto y = indicator_radius * std::cos(degree_offset * i);
    const auto growth = std::max(0.0f, std::sin(t * speed - i * degree_offset));
    ImVec4 color;
    color.x = main_color.x * growth + backdrop_color.x * (1.0f - growth);
    color.y = main_color.y * growth + backdrop_color.y * (1.0f - growth);
    color.z = main_color.z * growth + backdrop_color.z * (1.0f - growth);
    color.w = 1.0f;
    window->DrawList->AddCircleFilled(
        ImVec2(pos.x + bb_radius + x, pos.y + bb_radius - y),
        big_circle_radius * growth + small_circle_radius * (1.0f - growth),
        GetColorU32(color));
  }
}

}  // namespace imgui
}  // namespace typescript_cpp_v8
}  // namespace reify
