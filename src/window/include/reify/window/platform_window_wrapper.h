#ifndef _REIFY_WINDOW_PLATFORM_WINDOW_WRAPPER_H
#define _REIFY_WINDOW_PLATFORM_WINDOW_WRAPPER_H

#include <functional>
#include <memory>
#include <vector>

#include "reify/utils/error.h"
#include "reify/window/window.h"

namespace reify {
namespace window {

utils::MaybeError RunPlatformWindowWrapper(
    const std::string& window_title, std::unique_ptr<Window> domain_visualizer);

}  // namespace window
}  // namespace reify

#endif  // _REIFY_WINDOW_PLATFORM_WINDOW_WRAPPER_H
