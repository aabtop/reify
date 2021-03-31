#include "src/ide/web_interface.h"

#include <QMetaType>
#include <QWebEnginePage>

// static
void MonacoQtBridge::EnsureModuleListIsRegistered() {
  static int registered_pair = qRegisterMetaType<Module>("Module");
  static int registered_list = qRegisterMetaType<ModuleList>("ModuleList");
}

MonacoQtBridge::MonacoQtBridge(
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

void MonacoQtBridge::AddCompletionCallback(std::any&& callback) {
  completion_callbacks_.push(std::move(callback));
}

void MonacoQtBridge::WebChannelInitialized() {
  ModuleList qlist;
  for (const auto& module : typescript_input_modules_) {
    Module qmodule;
    qmodule.push_back(QString(std::string(module.path).c_str()));
    qmodule.push_back(QString(std::string(module.content).c_str()));
    qlist.push_back(qmodule);
  }
  emit TypeScriptWrapperConstructor(qlist);
}

void MonacoQtBridge::SaveAsReply(const QString& filepath,
                                 const QString& content) {
  std::any_cast<const SaveAsReplyFunction&>(completion_callbacks_.front())(
      filepath, content);
  completion_callbacks_.pop();
}

void MonacoQtBridge::QueryContentReply(const QString& content) {
  std::any_cast<const QueryContentReplyFunction&>(
      completion_callbacks_.front())(content);
  completion_callbacks_.pop();
}

WebInterface::WebInterface(
    QWebEnginePage* page,
    const std::vector<reify::CompilerEnvironment::InputModule>&
        typescript_input_modules,
    QWidget* parent)
    : bridge_(page, typescript_input_modules, parent) {}

void WebInterface::NewFile() { emit bridge_.NewFile(); }

void WebInterface::SaveAs(const QString& filepath,
                          const SaveAsReplyFunction& on_completion) {
  bridge_.AddCompletionCallback(on_completion);
  emit bridge_.SaveAs(filepath);
}

void WebInterface::Open(const QString& filepath, const QString& content) {
  emit bridge_.Open(filepath, content);
}

void WebInterface::QueryContent(
    const QueryContentReplyFunction& on_completion) {
  bridge_.AddCompletionCallback(on_completion);
  emit bridge_.QueryContent();
}