#ifndef _REIFY_TYPESCRIPT_CPP_V8_IDE_IDE_H
#define _REIFY_TYPESCRIPT_CPP_V8_IDE_IDE_H

#include <QWidget>
#include <functional>
#include <memory>
#include <vector>

#include "reify/typescript_cpp_v8/symbol_visualizer.h"

namespace reify {
namespace typescript_cpp_v8 {

int StartIdeWindow(const std::string& window_title,
                   SymbolVisualizer* symbol_visualizer,
                   const reify::pure_cpp::ThreadPoolCacheRunner& runner);

}  // namespace typescript_cpp_v8
}  // namespace reify

#endif  // _REIFY_TYPESCRIPT_CPP_V8_IDE_IDE_H
