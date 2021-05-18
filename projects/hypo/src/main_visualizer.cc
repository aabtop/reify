#include "reify/typescript_cpp_v8/visualizer_tool.h"
#include "src/domain_visualizer/domain_visualizer_hypo.h"

int main(int argc, char* argv[]) {
  return reify::typescript_cpp_v8::RunVisualizerTool(
      "Hypo Visualizer", std::make_unique<DomainVisualizerHypo>());
}
