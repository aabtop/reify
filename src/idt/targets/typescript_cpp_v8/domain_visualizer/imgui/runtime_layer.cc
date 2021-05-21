#include "reify/typescript_cpp_v8/imgui/runtime_layer.h"

#include "imgui.h"

namespace reify {
namespace typescript_cpp_v8 {
namespace imgui {

RuntimeLayer::RuntimeLayer() {}

void RuntimeLayer::ExecuteImGuiCommands() {
  ImGui::Begin("I AM A RUNTIME LAYER!!!");

  ImGui::End();
}

}  // namespace imgui
}  // namespace typescript_cpp_v8
}  // namespace reify
