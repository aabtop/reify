#ifndef _IDT_TARGETS_TYPESCRIPT_CPP_V8_DOMAIN_VISUALIZER_IMGUI_DOCKING_LAYER_H
#define _IDT_TARGETS_TYPESCRIPT_CPP_V8_DOMAIN_VISUALIZER_IMGUI_DOCKING_LAYER_H

#include <optional>

#include "imgui.h"

namespace reify {
namespace typescript_cpp_v8 {
namespace imgui {

class DockingLayer {
 public:
  DockingLayer();

  void ExecuteImGuiCommands();

  ImGuiID dock_main_id() const { return *dock_main_id_; }
  ImGuiID dock_right_id() const { return *dock_right_id_; }

 private:
  std::optional<ImGuiID> dock_main_id_;
  std::optional<ImGuiID> dock_right_id_;
};

}  // namespace imgui
}  // namespace typescript_cpp_v8
}  // namespace reify

#endif  // _IDT_TARGETS_TYPESCRIPT_CPP_V8_DOMAIN_VISUALIZER_IMGUI_DOCKING_LAYER_H
