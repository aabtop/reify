#include <qvulkaninstance.h>

#include "src/ide/domain_visualizer_hypo.h"
#include "src/ide/domain_visualizer_qt.h"

std::unique_ptr<DomainVisualizer> CreateDefaultQtWidgetDomainVisualizer(
    QWidget* frame) {
  QVulkanInstance inst;
  return std::make_unique<DomainVisualizerHypo>();
}
