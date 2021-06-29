#include "reify/typescript_cpp_v8/imgui/docking_freespace_to_window_viewport_layer.h"

#include <iostream>

#include "imgui.h"
#include "imgui_internal.h"

namespace reify {
namespace typescript_cpp_v8 {
namespace imgui {

void DockingFreespaceToWindowViewportLayer::ExecuteImGuiCommands() {
  ImGuiDockNode* node = docking_layer_->GetEmptySpaceNode();
  ImGuiViewport* viewport = ImGui::GetMainViewport();
  window::Rect new_viewport = [&] {
    if (!node) {
      return window::Rect{
          static_cast<int>(viewport->WorkPos.x),
          static_cast<int>(viewport->WorkPos.y),
          static_cast<int>(viewport->WorkPos.x + viewport->WorkSize.x),
          static_cast<int>(viewport->WorkPos.y + viewport->WorkSize.y),
      };
    } else {
      ImRect rect = node->Rect();
      return window::Rect{
          static_cast<int>(rect.Min.x),
          static_cast<int>(rect.Min.y),
          static_cast<int>(rect.Max.x),
          static_cast<int>(rect.Max.y),
      };
    }
  }();

  if (!window_viewport_->viewport() ||
      !(*window_viewport_->viewport() == new_viewport)) {
    window_viewport_->SetViewport(new_viewport);
  }
}

}  // namespace imgui
}  // namespace typescript_cpp_v8
}  // namespace reify
