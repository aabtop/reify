#ifndef _IDT_TARGETS_TYPESCRIPT_CPP_V8_DOMAIN_VISUALIZER_IMGUI_DOCKING_LAYER_H
#define _IDT_TARGETS_TYPESCRIPT_CPP_V8_DOMAIN_VISUALIZER_IMGUI_DOCKING_LAYER_H

#include <optional>

#include "imgui.h"
#include "imgui_internal.h"

namespace reify {
namespace typescript_cpp_v8 {
namespace imgui {

class DockingLayer {
 public:
  DockingLayer(ImGuiDir default_docked_direction,
               float default_docked_split_fraction)
      : default_docked_direction_(default_docked_direction),
        default_docked_split_fraction_(default_docked_split_fraction) {}

  void ExecuteImGuiCommands();

  ImGuiDockNode* GetEmptySpaceNode() const;
  ImGuiID GetDockedContentNodeId();
  ImGuiID GetDockedBottomNodeId();

  ImGuiID viewport_dock_id() const { return *viewport_dock_id_; };

 private:
  ImGuiID GetOrMakeDockedContentNodeIdForDirection(ImGuiDir docked_dir,
                                                   float split_fraction);
  ImGuiID SplitNodeAndReturnContent(ImGuiID parent_node, ImGuiDir docked_dir,
                                    float split_fraction);

  const ImGuiDir default_docked_direction_;
  const float default_docked_split_fraction_;
  std::optional<ImGuiID> viewport_dock_id_;
};

}  // namespace imgui
}  // namespace typescript_cpp_v8
}  // namespace reify

#endif  // _IDT_TARGETS_TYPESCRIPT_CPP_V8_DOMAIN_VISUALIZER_IMGUI_DOCKING_LAYER_H
