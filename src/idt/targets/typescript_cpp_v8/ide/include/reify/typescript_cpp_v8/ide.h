#ifndef _REIFY_TYPESCRIPT_CPP_V8_IDE_IDE_H
#define _REIFY_TYPESCRIPT_CPP_V8_IDE_IDE_H

#include <QWidget>
#include <functional>
#include <memory>
#include <vector>

#include "reify/pure_cpp/thread_pool_cache_runner.h"
#include "reify/typescript_cpp_v8/symbol_visualizer.h"

namespace reify {
namespace typescript_cpp_v8 {

int StartIdeWindow(const std::string& window_title,
                   SymbolVisualizer* symbol_visualizer,
                   reify::pure_cpp::ThreadPoolCacheRunner* runner);

}  // namespace typescript_cpp_v8
}  // namespace reify

#endif  // _REIFY_TYPESCRIPT_CPP_V8_IDE_IDE_H
