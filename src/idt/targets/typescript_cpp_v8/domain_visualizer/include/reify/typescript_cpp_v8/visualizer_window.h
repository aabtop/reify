#ifndef _REIFY_TYPESCRIPT_CPP_V8_DOMAIN_VISUALIZER_VISUALIZER_WINDOW_H
#define _REIFY_TYPESCRIPT_CPP_V8_DOMAIN_VISUALIZER_VISUALIZER_WINDOW_H

#include <functional>
#include <memory>
#include <vector>

#include "reify/typescript_cpp_v8/domain_visualizer.h"

namespace reify {
namespace typescript_cpp_v8 {

int StartVisualizerWindow(const std::string& window_title,
                          std::unique_ptr<DomainVisualizer> domain_visualizer);

}  // namespace typescript_cpp_v8
}  // namespace reify

#endif  // _REIFY_TYPESCRIPT_CPP_V8_DOMAIN_VISUALIZER_VISUALIZER_WINDOW_H
