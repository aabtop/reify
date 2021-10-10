#ifndef _IDT_TARGETS_TYPESCRIPT_CPP_V8_SYMBOL_VISUALIZER_VISUALIZER_TOOL_H
#define _IDT_TARGETS_TYPESCRIPT_CPP_V8_SYMBOL_VISUALIZER_VISUALIZER_TOOL_H

#include <memory>
#include <string>

#include "reify/pure_cpp/thread_pool_cache_runner.h"
#include "reify/typescript_cpp_v8/symbol_visualizer.h"
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
    const std::string& window_title, SymbolVisualizer* symbol_visualizer,
    reify::pure_cpp::ThreadPoolCacheRunner* runner,
    const VisualizerToolOptions& options = VisualizerToolOptions());

}  // namespace typescript_cpp_v8
}  // namespace reify

#endif  // _IDT_TARGETS_TYPESCRIPT_CPP_V8_SYMBOL_VISUALIZER_VISUALIZER_TOOL_H
