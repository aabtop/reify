#ifndef _IDT_TARGETS_TYPESCRIPT_CPP_V8_DOMAIN_VISUALIZER_VISUALIZER_TOOL_H
#define _IDT_TARGETS_TYPESCRIPT_CPP_V8_DOMAIN_VISUALIZER_VISUALIZER_TOOL_H

#include <memory>
#include <string>

#include "reify/typescript_cpp_v8/domain_visualizer.h"
#include "reify/utils/error.h"

namespace reify {
namespace typescript_cpp_v8 {

struct VisualizerToolOptions {
  std::optional<std::filesystem::path> project_path;
};

std::variant<int, VisualizerToolOptions> ParseVisualizerToolOptions(
    const std::string& app_name, const std::string& app_description, int argc,
    char* argv[]);

utils::MaybeError RunVisualizerTool(
    const std::string& window_title,
    std::unique_ptr<DomainVisualizer> domain_visualizer,
    const VisualizerToolOptions& options = VisualizerToolOptions());

}  // namespace typescript_cpp_v8
}  // namespace reify

#endif  // _IDT_TARGETS_TYPESCRIPT_CPP_V8_DOMAIN_VISUALIZER_VISUALIZER_TOOL_H
