#include "reify/typescript_cpp_v8/imgui/status_layer.h"

#include <sstream>

#include "imgui.h"
#include "imgui_internal.h"
#include "reify/typescript_cpp_v8/imgui/widgets.h"
namespace reify {
namespace typescript_cpp_v8 {
namespace imgui {

StatusLayer::StatusLayer() {}

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
  const ImGuiStyle& style = ImGui::GetStyle();
  ImGui::SetWindowPos(ImVec2(viewport->Pos.x + viewport->CurrWorkOffsetMin.x +
                                 style.DisplaySafeAreaPadding.x,
                             viewport->Pos.y + viewport->Size.y +
                                 viewport->CurrWorkOffsetMax.y - height -
                                 style.DisplaySafeAreaPadding.y));

  ImGui::End();
}

}  // namespace imgui
}  // namespace typescript_cpp_v8
}  // namespace reify
