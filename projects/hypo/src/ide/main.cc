#include <QWidget>

#include "reify/typescript_cpp_v8/ide.h"
#include "src/ide/domain_visualizer_hypo_qt.h"

int main(int argc, char* argv[]) {
  return reify::typescript_cpp_v8::StartIdeWindow(
      "Hypo IDE", [](QWidget* parent) {
        return std::make_unique<DomainVisualizerHypoQtVulkan>(parent);
      });
}
