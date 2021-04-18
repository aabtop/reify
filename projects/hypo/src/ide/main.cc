#include <QWidget>

#include "reify/typescript_cpp_v8/ide.h"
#include "src/ide/domain_visualizer_hypo.h"

int main(int argc, char* argv[]) {
  return reify::typescript_cpp_v8::StartIdeWindow(
      "Hypo IDE", std::make_unique<DomainVisualizerHypo>());
}
