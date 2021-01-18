#ifndef _IDE_DOMAIN_VISUALIZER_QT_H
#define _IDE_DOMAIN_VISUALIZER_QT_H

#include <qwidget.h>

#include <memory>

#include "src/ide/domain_visualizer.h"

std::unique_ptr<DomainVisualizer> CreateDefaultQtWidgetDomainVisualizer(
    QWidget* frame);

#endif  // _IDE_DOMAIN_VISUALIZER_QT_H
