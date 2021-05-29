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
  DockingLayer(ImGuiDir default_docked_direction)
      : default_docked_direction_(default_docked_direction) {}

  void ExecuteImGuiCommands();

  ImGuiDockNode* GetEmptySpaceNodeId() const;
  ImGuiID viewport_dock_id() const { return *viewport_dock_id_; };
  ImGuiID dock_main_id() const { return *dock_main_id_; }
  ImGuiID dock_right_id() const { return *dock_right_id_; }

 private:
  const ImGuiDir default_docked_direction_;
  std::optional<ImGuiID> viewport_dock_id_;
  std::optional<ImGuiID> dock_main_id_;
  std::optional<ImGuiID> dock_right_id_;
};

}  // namespace imgui
}  // namespace typescript_cpp_v8
}  // namespace reify

#endif  // _IDT_TARGETS_TYPESCRIPT_CPP_V8_DOMAIN_VISUALIZER_IMGUI_DOCKING_LAYER_H
