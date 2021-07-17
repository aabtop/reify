#ifndef _HYPO_VISUALIZER_VISUALIZER_H
#define _HYPO_VISUALIZER_VISUALIZER_H

#include "reify/typescript_cpp_v8/hypo.h"
#include "reify/typescript_cpp_v8/symbol_visualizer.h"
#include "src/visualizer/object_visualizer_region3.h"

namespace hypo {

class TypeScriptSymbolVisualizer {
 public:
  TypeScriptSymbolVisualizer()
      : visualizer(reify::typescript_cpp_v8::hypo::typescript_declarations(),
                   {*reify::typescript_cpp_v8::MakeTypeScriptSymbolVisualizer(
                       &region_3_visualizer)}) {}

  ObjectVisualizerRegion3 region_3_visualizer;
  reify::typescript_cpp_v8::SymbolVisualizer visualizer;
};

}  // namespace hypo

#endif  // _HYPO_VISUALIZER_VISUALIZER_H
