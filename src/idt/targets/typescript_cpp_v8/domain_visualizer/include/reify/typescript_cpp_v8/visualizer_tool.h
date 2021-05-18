#ifndef _IDT_TARGETS_TYPESCRIPT_CPP_V8_DOMAIN_VISUALIZER_VISUALIZER_TOOL_H
#define _IDT_TARGETS_TYPESCRIPT_CPP_V8_DOMAIN_VISUALIZER_VISUALIZER_TOOL_H

#include <memory>
#include <string>

#include "reify/typescript_cpp_v8/domain_visualizer.h"

namespace reify {
namespace typescript_cpp_v8 {

int RunVisualizerTool(const std::string& window_title,
                      std::unique_ptr<DomainVisualizer> domain_visualizer);

}  // namespace typescript_cpp_v8
}  // namespace reify

#endif  // _IDT_TARGETS_TYPESCRIPT_CPP_V8_DOMAIN_VISUALIZER_VISUALIZER_TOOL_H
