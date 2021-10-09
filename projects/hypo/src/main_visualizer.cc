#include <iostream>

#include "reify/typescript_cpp_v8/visualizer_tool.h"
#include "reify/utils/error.h"
#include "src/visualizer/typescript_symbol_visualizer.h"

int main(int argc, char* argv[]) {
  std::string APP_NAME = "Hypo Visualizer";
  REIFY_UTILS_ASSIGN_OR_RETURN(
      options, reify::typescript_cpp_v8::ParseVisualizerToolOptions(
                   APP_NAME, "Visualizer for Hypo TypeScript-defined geometry.",
                   argc, argv));

  hypo::visualizer::TypeScriptSymbolVisualizer hypo_visualizer_stack;
  auto maybe_error = reify::typescript_cpp_v8::RunVisualizerTool(
      APP_NAME, &hypo_visualizer_stack.visualizer, hypo_visualizer_stack.runner,
      options);

  if (maybe_error) {
    std::cerr << "Error: " << maybe_error->msg << std::endl;
    return 1;
  }

  return 0;
}
