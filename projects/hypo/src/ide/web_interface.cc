#include "src/ide/web_interface.h"

#include <iostream>

WebInterface::WebInterface(QWidget* parent) : QObject(parent) {}

void WebInterface::SaveAsReply(const QString& filepath,
                               const QString& content) {
  emit OnSaveAsReply(filepath, content);
}