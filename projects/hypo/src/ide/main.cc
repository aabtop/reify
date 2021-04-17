#include <QWidget>

#include "domain_visualizer_hypo_qt.h"
#include "reify/typescript_cpp_v8/ide.h"

int main(int argc, char* argv[]) {
  return reify::typescript_cpp_v8::StartIdeWindow(
      "Hypo IDE", [](QWidget* parent) {
        return std::make_unique<DomainVisualizerHypoQtVulkan>(parent);
      });
}
