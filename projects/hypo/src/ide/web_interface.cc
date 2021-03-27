#include "src/ide/web_interface.h"

WebInterface::WebInterface(QWidget* parent) : QObject(parent) {}

void WebInterface::SaveAsReply(const QString& filepath,
                               const QString& content) {
  emit OnSaveAsReply(filepath, content);
}

void WebInterface::QueryContentReply(const QString& content) {
  emit OnQueryContentReply(content);
}