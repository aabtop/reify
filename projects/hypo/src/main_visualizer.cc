#include "reify/typescript_cpp_v8/domain_visualizer_gui.h"
#include "reify/typescript_cpp_v8/visualizer_window.h"
#include "src/domain_visualizer/domain_visualizer_hypo.h"

int main(int argc, char* argv[]) {
  return reify::typescript_cpp_v8::StartVisualizerWindow(
      "Hypo Visualizer",
      std::make_unique<reify::typescript_cpp_v8::DomainVisualizerGui>(
          std::make_unique<DomainVisualizerHypo>()));
}
