#ifndef _REIFY_WINDOW_PLATFORM_WINDOW_WRAPPER_H
#define _REIFY_WINDOW_PLATFORM_WINDOW_WRAPPER_H

#include <functional>
#include <memory>
#include <vector>

#include "reify/window/window.h"

namespace reify {
namespace window {

int RunPlatformWindowWrapper(const std::string& window_title,
                             std::unique_ptr<Window> domain_visualizer);

}  // namespace window
}  // namespace reify

#endif  // _REIFY_WINDOW_PLATFORM_WINDOW_WRAPPER_H
