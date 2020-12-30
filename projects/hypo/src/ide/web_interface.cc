#include "src/ide/web_interface.h"

WebInterface::WebInterface(QWidget* parent) : QObject(parent) {}

void WebInterface::ChangeData(const QString& data) { emit dataChanged(data); }

void WebInterface::generateRandomData() { ChangeData("Foobar"); }
