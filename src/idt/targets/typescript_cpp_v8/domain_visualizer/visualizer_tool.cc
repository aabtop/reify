#include "reify/typescript_cpp_v8/visualizer_tool.h"

#include <filesystem>

#include "CLI/CLI.hpp"
#include "reify/typescript_cpp_v8/domain_visualizer.h"
#include "reify/typescript_cpp_v8/domain_visualizer_gui.h"
#include "reify/window/platform_window_wrapper.h"

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
    std::unique_ptr<DomainVisualizer> domain_visualizer,
    const VisualizerToolOptions& options) {
  auto wrapped_domain_visualizer =
      std::make_unique<reify::typescript_cpp_v8::DomainVisualizerGui>(
          std::move(domain_visualizer));

  return window::RunPlatformWindowWrapper(window_title,
                                          std::move(wrapped_domain_visualizer));
}

}  // namespace typescript_cpp_v8
}  // namespace reify
