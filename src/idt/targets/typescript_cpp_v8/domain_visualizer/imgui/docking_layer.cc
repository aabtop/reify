#include "reify/typescript_cpp_v8/imgui/docking_layer.h"

#include <sstream>

#include "imgui.h"
#include "imgui_internal.h"

namespace reify {
namespace typescript_cpp_v8 {
namespace imgui {

DockingLayer::DockingLayer() {}

namespace {
ImGuiDockNode* FindFirstEmptySpaceNode(ImGuiDockNode* root) {
  if (!root) {
    return nullptr;
  }

  if (root->IsEmpty()) {
    return root;
  }

  for (int i = 0; i < 2; ++i) {
    ImGuiDockNode* recursive_result =
        FindFirstEmptySpaceNode(root->ChildNodes[i]);
    if (recursive_result != nullptr) {
      return recursive_result;
    }
  }

  return nullptr;
}
}  // namespace

ImGuiDockNode* DockingLayer::GetEmptySpaceNodeId() const {
  return FindFirstEmptySpaceNode(ImGui::DockBuilderGetNode(viewport_dock_id()));
}

void DockingLayer::ExecuteImGuiCommands() {
  viewport_dock_id_ = ImGui::DockSpaceOverViewport(
      nullptr, ImGuiDockNodeFlags_PassthruCentralNode);
  if (!dock_right_id_) {
    dock_right_id_ = ImGui::DockBuilderSplitNode(
        viewport_dock_id(), ImGuiDir_Right, 0.2f, nullptr, &(*dock_main_id_));
  }
}

}  // namespace imgui
}  // namespace typescript_cpp_v8
}  // namespace reify
