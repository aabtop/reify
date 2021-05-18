#include <iostream>

#include "reify/typescript_cpp_v8/visualizer_tool.h"
#include "reify/utils/error.h"
#include "src/domain_visualizer/domain_visualizer_hypo.h"

int main(int argc, char* argv[]) {
  std::string APP_NAME = "Hypo Visualizer";
  REIFY_UTILS_ASSIGN_OR_RETURN(
      options, reify::typescript_cpp_v8::ParseVisualizerToolOptions(
                   APP_NAME, "Visualizer for Hypo TypeScript-defined geometry.",
                   argc, argv));

  auto maybe_error = reify::typescript_cpp_v8::RunVisualizerTool(
      APP_NAME, std::make_unique<DomainVisualizerHypo>(), options);

  if (maybe_error) {
    std::cerr << "Error: " << maybe_error->msg << std::endl;
    return 1;
  }

  return 0;
}
