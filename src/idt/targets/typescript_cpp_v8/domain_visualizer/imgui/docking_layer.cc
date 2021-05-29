#include "reify/typescript_cpp_v8/imgui/docking_layer.h"

#include <sstream>

#include "imgui.h"
#include "imgui_internal.h"

namespace reify {
namespace typescript_cpp_v8 {
namespace imgui {

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

ImGuiDockNode* DockingLayer::GetEmptySpaceNode() const {
  return FindFirstEmptySpaceNode(ImGui::DockBuilderGetNode(viewport_dock_id()));
}

namespace {
ImGuiAxis axis_for_direction(ImGuiDir dir) {
  switch (dir) {
    case ImGuiDir_Left:
      return ImGuiAxis_X;
    case ImGuiDir_Up:
      return ImGuiAxis_Y;
    case ImGuiDir_Right:
      return ImGuiAxis_X;
    case ImGuiDir_Down:
      return ImGuiAxis_Y;
  }
  assert(false);
  return ImGuiAxis_None;
}

int child_index_for_direction(ImGuiDir dir) {
  switch (dir) {
    case ImGuiDir_Left:
      return 0;
    case ImGuiDir_Up:
      return 0;
    case ImGuiDir_Right:
      return 1;
    case ImGuiDir_Down:
      return 1;
  }
  assert(false);
  return ImGuiDir_None;
}

int other_child_index(int child_index) { return 1 - child_index; }
}  // namespace

// Find the empty space node, and return a node directly in the
// `default_docked_direction_` direction of it, creating it if it doesn't exist.
ImGuiID DockingLayer::GetDockedContentNodeId() const {
  ImGuiDockNode* empty_space_node = GetEmptySpaceNode();

  ImGuiAxis default_split_axis = axis_for_direction(default_docked_direction_);

  if (empty_space_node->ParentNode &&
      empty_space_node->ParentNode->SplitAxis == default_split_axis) {
    // We can re-use the existing split here.
    ImGuiDockNode* content_dock_node =
        empty_space_node->ParentNode
            ->ChildNodes[child_index_for_direction(default_docked_direction_)];
    if (empty_space_node != content_dock_node &&
        content_dock_node->IsLeafNode()) {
      return content_dock_node->ID;
    }
  }

  // Okay, we can split this node and and create a new content pane node to
  // return.
  return SplitNodeAndReturnContent(empty_space_node->ID);
}

void DockingLayer::ExecuteImGuiCommands() {
  viewport_dock_id_ = ImGui::DockSpaceOverViewport(
      nullptr, ImGuiDockNodeFlags_PassthruCentralNode |
                   ImGuiDockNodeFlags_NoDockingInCentralNode);
}

ImGuiID DockingLayer::SplitNodeAndReturnContent(ImGuiID parent_node) const {
  return ImGui::DockBuilderSplitNode(parent_node, default_docked_direction_,
                                     default_docked_split_fraction_, nullptr,
                                     nullptr);
}

}  // namespace imgui
}  // namespace typescript_cpp_v8
}  // namespace reify
