#include "reify/typescript_cpp_v8/visualizer_tool.h"

#include <filesystem>

#include "CLI/CLI.hpp"
#include "imgui.h"
#include "imgui_internal.h"
#include "reify/typescript_cpp_v8/imgui/docking_freespace_to_window_viewport_layer.h"
#include "reify/typescript_cpp_v8/imgui/docking_layer.h"
#include "reify/typescript_cpp_v8/imgui/layer_stack.h"
#include "reify/typescript_cpp_v8/imgui/project_layer.h"
#include "reify/typescript_cpp_v8/imgui/runtime_layer.h"
#include "reify/typescript_cpp_v8/imgui/status_layer.h"
#include "reify/typescript_cpp_v8/symbol_visualizer.h"
#include "reify/typescript_cpp_v8/typescript_cpp_v8.h"
#include "reify/utils/thread_with_work_queue.h"
#include "reify/window/platform_window_wrapper.h"
#include "reify/window/window_stack.h"
#include "reify/window/window_viewport.h"

namespace reify {
namespace typescript_cpp_v8 {

std::variant<int, VisualizerToolOptions> ParseVisualizerToolOptions(
    const std::string& app_name, const std::string& app_description, int argc,
    char* argv[]) {
  VisualizerToolOptions options;

  CLI::App app{app_description, app_name};

  app.add_option(
         "input_typescript_project_path,-i,--input_typescript_project_path",
         options.project_path,
         "Path to a TypeScript file or project directory.")
      ->check(CLI::ExistingPath);

  try {
    app.parse(argc, argv);
  } catch (const CLI::ParseError& e) {
    return app.exit(e);
  }

  return options;
}

utils::MaybeError RunVisualizerTool(const std::string& window_title,
                                    SymbolVisualizer* symbol_visualizer,
                                    const VisualizerToolOptions& options) {
  utils::ThreadWithWorkQueue visualizer_thread;

  window::WindowViewport symbol_visualizer_viewport(symbol_visualizer);

  imgui::DockingLayer docking_layer(ImGuiDir_Right, 0.2f);
  imgui::DockingFreespaceToWindowViewportLayer
      docking_freespace_to_window_viewport_layer(&symbol_visualizer_viewport,
                                                 &docking_layer);
  imgui::StatusLayer status_layer(&docking_layer);
  imgui::RuntimeLayer runtime_layer(
      [&visualizer_thread](auto x) { visualizer_thread.Enqueue(x); },
      &docking_layer, &status_layer, symbol_visualizer);
  imgui::ProjectLayer project_layer(&visualizer_thread, &status_layer,
                                    &runtime_layer, options.project_path);
  imgui::LayerStack imgui_layer_stack(
      {[&docking_layer] { docking_layer.ExecuteImGuiCommands(); },
       [&runtime_layer] { runtime_layer.ExecuteImGuiCommands(); },
       [&project_layer] { project_layer.ExecuteImGuiCommands(); },
       [&status_layer] { status_layer.ExecuteImGuiCommands(); },
       [&docking_freespace_to_window_viewport_layer] {
         docking_freespace_to_window_viewport_layer.ExecuteImGuiCommands();
       }});

  window::WindowStack window_stack(
      {&symbol_visualizer_viewport, &imgui_layer_stack});
  return window::RunPlatformWindowWrapper(window_title, &window_stack,
                                          &visualizer_thread);
}

}  // namespace typescript_cpp_v8
}  // namespace reify