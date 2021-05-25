#ifndef _REIFY_WINDOW_PLATFORM_WINDOW_WRAPPER_H
#define _REIFY_WINDOW_PLATFORM_WINDOW_WRAPPER_H

#include <functional>
#include <memory>
#include <vector>

#include "reify/utils/error.h"
#include "reify/utils/thread_with_work_queue.h"
#include "reify/window/window.h"

namespace reify {
namespace window {

utils::MaybeError RunPlatformWindowWrapper(
    const std::string& window_title, Window* domain_visualizer,
    utils::ThreadWithWorkQueue* wrapped_window_thread);

}  // namespace window
}  // namespace reify

#endif  // _REIFY_WINDOW_PLATFORM_WINDOW_WRAPPER_H
