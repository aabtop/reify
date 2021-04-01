#include "src/ide/monaco_interface.h"

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
    const std::function<void()>& on_initialization_complete, QWidget* parent)
    : QObject(parent),
      typescript_input_modules_(typescript_input_modules),
      web_channel_(page),
      on_initialization_complete_(on_initialization_complete) {
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

  initialized_ = true;
  (*on_initialization_complete_)();
  on_initialization_complete_ = std::nullopt;
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

MonacoInterface::MonacoInterface(
    QWebEnginePage* page,
    const std::vector<reify::CompilerEnvironment::InputModule>&
        typescript_input_modules,
    const std::function<void()>& on_initialization_complete, QWidget* parent)
    : bridge_(page, typescript_input_modules, on_initialization_complete,
              parent) {}

void MonacoInterface::NewFile() { emit bridge_.NewFile(); }

void MonacoInterface::SaveAs(const QString& filepath,
                             const SaveAsReplyFunction& on_completion) {
  bridge_.AddCompletionCallback(on_completion);
  emit bridge_.SaveAs(filepath);
}

void MonacoInterface::Open(const QString& filepath, const QString& content) {
  emit bridge_.Open(filepath, content);
}

void MonacoInterface::QueryContent(
    const QueryContentReplyFunction& on_completion) {
  bridge_.AddCompletionCallback(on_completion);
  emit bridge_.QueryContent();
}