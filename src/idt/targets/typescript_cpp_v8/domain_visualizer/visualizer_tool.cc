#include "reify/typescript_cpp_v8/visualizer_tool.h"

#include <filesystem>

#include "CLI/CLI.hpp"
#include "reify/typescript_cpp_v8.h"
#include "reify/typescript_cpp_v8/domain_visualizer.h"
#include "reify/typescript_cpp_v8/imgui/layer_stack.h"
#include "reify/typescript_cpp_v8/imgui/project_layer.h"
#include "reify/typescript_cpp_v8/imgui/runtime_layer.h"
#include "reify/typescript_cpp_v8/imgui/status_layer.h"
#include "reify/utils/thread_with_work_queue.h"
#include "reify/window/platform_window_wrapper.h"
#include "reify/window/window_stack.h"

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

utils::MaybeError RunVisualizerTool(
    const std::string& window_title,
    const std::function<std::unique_ptr<DomainVisualizer>()>&
        create_domain_visualizer,
    const VisualizerToolOptions& options) {
  utils::ThreadWithWorkQueue visualizer_thread;

  std::unique_ptr<DomainVisualizer> domain_visualizer =
      create_domain_visualizer();

  imgui::StatusLayer status_layer;
  imgui::RuntimeLayer runtime_layer(
      [&visualizer_thread](auto x) { visualizer_thread.Enqueue(x); },
      &status_layer, domain_visualizer.get());
  imgui::ProjectLayer project_layer(&visualizer_thread, &status_layer,
                                    &runtime_layer, options.project_path);
  imgui::LayerStack imgui_layer_stack({
      [&runtime_layer]() { runtime_layer.ExecuteImGuiCommands(); },
      [&project_layer]() { project_layer.ExecuteImGuiCommands(); },
      [&status_layer]() { status_layer.ExecuteImGuiCommands(); },
  });

  window::WindowStack window_stack(
      {domain_visualizer.get(), &imgui_layer_stack});
  return window::RunPlatformWindowWrapper(window_title, &window_stack,
                                          &visualizer_thread);
}

}  // namespace typescript_cpp_v8
}  // namespace reify
