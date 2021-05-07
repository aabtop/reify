#include "reify/typescript_cpp_v8/visualizer_window.h"

#include <vulkan/vulkan.h>

#include <iostream>

#include "platform_window/platform_window.h"

namespace reify {
namespace typescript_cpp_v8 {

int StartVisualizerWindow(const std::string& window_title,
                          std::unique_ptr<DomainVisualizer> domain_visualizer) {
  bool should_quit = false;
  PlatformWindow window = PlatformWindowMakeDefaultWindow(
      "foobar",
      [](void* context, PlatformWindowEvent event, void* event_data) {
        bool* should_quit = static_cast<bool*>(context);
        if (event == kPlatformWindowEventQuitRequest) {
          *should_quit = true;
        }
      },
      &should_quit);

  while (!should_quit) {
  }

  std::cerr << "Done!" << std::endl;

  PlatformWindowDestroyWindow(window);

  return 0;
}

}  // namespace typescript_cpp_v8
}  // namespace reify
