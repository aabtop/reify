#include "reify/typescript_cpp_v8/imgui/docking_layer.h"

#include <sstream>

#include "imgui.h"
#include "imgui_internal.h"

namespace reify {
namespace typescript_cpp_v8 {
namespace imgui {

DockingLayer::DockingLayer() {}

void DockingLayer::ExecuteImGuiCommands() {
  auto viewport_dockspace_id = ImGui::DockSpaceOverViewport(
      nullptr, ImGuiDockNodeFlags_PassthruCentralNode);
  if (!dock_right_id_) {
    dock_right_id_ =
        ImGui::DockBuilderSplitNode(viewport_dockspace_id, ImGuiDir_Right, 0.2f,
                                    nullptr, &(*dock_main_id_));
  }
}

}  // namespace imgui
}  // namespace typescript_cpp_v8
}  // namespace reify
