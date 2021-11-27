#include "reify/typescript_cpp_v8/imgui/docking_freespace_to_window_viewport_layer.h"

#include <iostream>

#include "imgui.h"
#include "imgui_internal.h"

namespace reify {
namespace typescript_cpp_v8 {
namespace imgui {

void DockingFreespaceToWindowViewportLayer::ExecuteImGuiCommands() {
  if (!window_viewport_->get_on_viewport_resize_size()) {
    return;
  }
  // The resize size set externally on the viewport represents the same
  // coordinate system that the mouse events will be arriving in. Since this
  // might be different than the framebuffer/imgui coordinate systems, we scale
  // those accordingly so that mouse events match up.
  std::array<int, 2> on_viewport_resize_size =
      *window_viewport_->get_on_viewport_resize_size();
  ImVec2 imgui_display_size = ImGui::GetIO().DisplaySize;
  std::array<float, 2> scaling = {
      on_viewport_resize_size[0] / imgui_display_size[0],
      on_viewport_resize_size[1] / imgui_display_size[1],
  };

  ImGuiDockNode* node = docking_layer_->GetEmptySpaceNode();
  ImGuiViewport* viewport = ImGui::GetMainViewport();
  window::Rect new_viewport = [&] {
    if (!node) {
      return window::Rect{
          static_cast<int>(viewport->WorkPos.x * scaling[0]),
          static_cast<int>(viewport->WorkPos.y * scaling[1]),
          static_cast<int>((viewport->WorkPos.x + viewport->WorkSize.x) *
                           scaling[0]),
          static_cast<int>((viewport->WorkPos.y + viewport->WorkSize.y) *
                           scaling[1]),
      };
    } else {
      ImRect rect = node->Rect();
      return window::Rect{
          static_cast<int>(rect.Min.x * scaling[0]),
          static_cast<int>(rect.Min.y * scaling[1]),
          static_cast<int>(rect.Max.x * scaling[0]),
          static_cast<int>(rect.Max.y * scaling[1]),
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
