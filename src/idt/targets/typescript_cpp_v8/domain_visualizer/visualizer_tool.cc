#include "reify/typescript_cpp_v8/visualizer_tool.h"

#include "reify/typescript_cpp_v8/domain_visualizer.h"
#include "reify/typescript_cpp_v8/domain_visualizer_gui.h"
#include "reify/window/platform_window_wrapper.h"

namespace reify {
namespace typescript_cpp_v8 {

int RunVisualizerTool(const std::string& window_title,
                      std::unique_ptr<DomainVisualizer> domain_visualizer) {
  // In the future we can insert additional layers here.
  return window::RunPlatformWindowWrapper(
      window_title,
      std::make_unique<reify::typescript_cpp_v8::DomainVisualizerGui>(
          std::move(domain_visualizer)));
}

}  // namespace typescript_cpp_v8
}  // namespace reify
