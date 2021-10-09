#include <QWidget>

#include "reify/typescript_cpp_v8/ide.h"
#include "src/visualizer/typescript_symbol_visualizer.h"

int main(int argc, char* argv[]) {
  hypo::visualizer::TypeScriptSymbolVisualizer hypo_visualizer_stack;
  return reify::typescript_cpp_v8::StartIdeWindow(
      "Hypo IDE", &hypo_visualizer_stack.visualizer,
      hypo_visualizer_stack.runner);
}
