#include "reify/typescript_cpp_v8/imgui/docking_freespace_to_window_viewport_layer.h"

#include "imgui.h"
#include "imgui_internal.h"

namespace reify {
namespace typescript_cpp_v8 {
namespace imgui {

void DockingFreespaceToWindowViewportLayer::ExecuteImGuiCommands() {
  ImGuiDockNode* node =
      ImGui::DockBuilderGetNode(docking_layer_->dock_main_id());
  ImGuiViewport* viewport = ImGui::GetMainViewport();
  if (!node) {
    window_viewport_->SetViewport({
        static_cast<int>(viewport->WorkPos.x),
        static_cast<int>(viewport->WorkPos.y),
        static_cast<int>(viewport->WorkPos.x + viewport->WorkSize.x),
        static_cast<int>(viewport->WorkPos.y + viewport->WorkSize.y),
    });
  } else {
    ImRect rect = node->Rect();
    window_viewport_->SetViewport({
        static_cast<int>(rect.Min.x + viewport->WorkPos.x),
        static_cast<int>(rect.Min.y + viewport->WorkPos.y),
        static_cast<int>(rect.Max.x + viewport->WorkPos.x),
        static_cast<int>(rect.Max.y + viewport->WorkPos.y),
    });
  }
}

}  // namespace imgui
}  // namespace typescript_cpp_v8
}  // namespace reify
