#include "reify/typescript_cpp_v8/imgui/status_layer.h"

#include <sstream>

#include "imgui.h"
#include "imgui_internal.h"

namespace reify {
namespace typescript_cpp_v8 {
namespace imgui {

StatusLayer::StatusLayer(DockingLayer* docking_layer)
    : docking_layer_(docking_layer) {}

void StatusLayer::Register(Window* window) { windows_.push_back(window); }
void StatusLayer::Unregister(Window* window) {
  auto found = std::find(windows_.begin(), windows_.end(), window);
  windows_.erase(found);
}

void StatusLayer::ExecuteImGuiCommands() {
  if (windows_.empty()) {
    return;
  }

  ImGuiWindowFlags status_main_window_flags =
      ImGuiWindowFlags_NoTitleBar | ImGuiWindowFlags_NoMove |
      ImGuiWindowFlags_NoResize | ImGuiWindowFlags_NoScrollbar |
      ImGuiWindowFlags_NoSavedSettings;
  ImGui::SetNextWindowSize(ImVec2(0, 0));
  ImGui::PushStyleVar(ImGuiStyleVar_WindowRounding, 12.0f);
  ImGui::Begin("Status Overlay", nullptr, status_main_window_flags);
  ImGui::PopStyleVar();

  for (const auto& window : windows_) {
    ImGui::BeginGroup();
    window->draw_function()();
    ImGui::EndGroup();
  }

  float height = ImGui::GetWindowHeight();
  ImGuiViewportP* viewport = (ImGuiViewportP*)(void*)ImGui::GetMainViewport();
  ImRect empty_space_rect = docking_layer_->GetEmptySpaceNode()->Rect();
  const ImGuiStyle& style = ImGui::GetStyle();

  const float offset_from_corner = 5;
  ImGui::SetWindowPos(
      ImVec2(empty_space_rect.Min.x + style.DisplaySafeAreaPadding.x +
                 offset_from_corner,
             empty_space_rect.Max.y - style.DisplaySafeAreaPadding.y - height -
                 offset_from_corner));

  ImGui::End();
}

}  // namespace imgui
}  // namespace typescript_cpp_v8
}  // namespace reify
