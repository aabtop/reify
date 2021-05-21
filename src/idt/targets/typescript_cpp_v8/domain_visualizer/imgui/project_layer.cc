#include "reify/typescript_cpp_v8/imgui/project_layer.h"

#include "imgui.h"

namespace reify {
namespace typescript_cpp_v8 {
namespace imgui {

void ProjectLayer::ExecuteImGuiCommands() {
  ImGui::Begin("Hello, world!");

  ImGui::Text("This is some useful text.");
  ImGui::Checkbox("Demo Window", &checkbox_);

  ImGui::SliderFloat("float", &f_, 0.0f, 1.0f);

  if (ImGui::Button("Button")) {
    counter_++;
  }
  ImGui::SameLine();
  ImGui::Text("counter = %d", counter_);

  ImGui::Text("Application average %.3f ms/frame (%.1f FPS)",
              1000.0f / ImGui::GetIO().Framerate, ImGui::GetIO().Framerate);
  ImGui::End();
}

}  // namespace imgui
}  // namespace typescript_cpp_v8
}  // namespace reify
