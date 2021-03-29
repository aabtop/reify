#include "src/ide/web_interface.h"

#include <QMetaType>
#include <QWebEnginePage>

// static
void WebInterface::EnsureModuleListIsRegistered() {
  static int registered_pair = qRegisterMetaType<Module>("Module");
  static int registered_list = qRegisterMetaType<ModuleList>("ModuleList");
}

WebInterface::WebInterface(
    QWebEnginePage* page,
    const std::vector<reify::CompilerEnvironment::InputModule>&
        typescript_input_modules,
    QWidget* parent)
    : QObject(parent),
      typescript_input_modules_(typescript_input_modules),
      web_channel_(page) {
  page->setWebChannel(&web_channel_);

  web_channel_.registerObject(QStringLiteral("monaco_qt_bridge"), this);
  EnsureModuleListIsRegistered();
}

void WebInterface::WebChannelInitialized() {
  ModuleList qlist;
  for (const auto& module : typescript_input_modules_) {
    Module qmodule;
    qmodule.push_back(QString(std::string(module.path).c_str()));
    qmodule.push_back(QString(std::string(module.content).c_str()));
    qlist.push_back(qmodule);
  }
  emit TypeScriptWrapperConstructor(qlist);
}

void WebInterface::SaveAsReply(const QString& filepath,
                               const QString& content) {
  emit OnSaveAsReply(filepath, content);
}

void WebInterface::QueryContentReply(const QString& content) {
  emit OnQueryContentReply(content);
}