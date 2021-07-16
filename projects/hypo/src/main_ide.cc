#include <QWidget>

#include "reify/typescript_cpp_v8/ide.h"
#include "src/domain_visualizer/domain_visualizer_hypo.h"

int main(int argc, char* argv[]) {
  HypoTypeScriptSymbolVisualizerStack hypo_visualizer_stack;
  return reify::typescript_cpp_v8::StartIdeWindow(
      "Hypo IDE", &hypo_visualizer_stack.visualizer);
}
