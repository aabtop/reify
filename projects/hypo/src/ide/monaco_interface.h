#ifndef WEB_INTERFACE_H_
#define WEB_INTERFACE_H_

#include <QFuture>
#include <QList>
#include <QObject>
#include <QPair>
#include <QWebChannel>
#include <QWebEnginePage>
#include <QWidget>
#include <any>
#include <queue>

#include "reify/typescript_cpp_v8.h"

class MonacoQtBridge : public QObject {
  Q_OBJECT

 public:
  MonacoQtBridge(QWebEnginePage* page,
                 const std::vector<reify::CompilerEnvironment::InputModule>&
                     typescript_input_modules,
                 const std::function<void()>& on_initialization_complete,
                 QWidget* parent);

  using SaveAsReplyFunction =
      std::function<void(const QString&, const QString&)>;
  using QueryContentReplyFunction = std::function<void(const QString&)>;

  void AddCompletionCallback(std::any&& callback);

  bool initialized() const { return initialized_; }

 private:
  using Module = QList<QString>;  // A pair of strings: content and filename.
  using ModuleList = QList<Module>;
  static void EnsureModuleListIsRegistered();

 public slots:
  void WebChannelInitialized();

  void SaveAsReply(const QString& filepath,
                   const QString& content);  // Called by the QWebEngine.

  void QueryContentReply(const QString& content);  // Called by the QWebEngine.

 signals:
  // Signals emitted by C++.
  void NewFile();
  void SaveAs(const QString& filepath);
  void Open(const QString& filepath, const QString& content);
  void QueryContent();

  void TypeScriptWrapperConstructor(const ModuleList& modules);

 private:
  QWebChannel web_channel_;

  const std::vector<reify::CompilerEnvironment::InputModule>
      typescript_input_modules_;

  std::queue<std::any> completion_callbacks_;

  bool initialized_ = false;
  std::optional<std::function<void()>> on_initialization_complete_;
};

class MonacoInterface {
 public:
  MonacoInterface(QWebEnginePage* page,
                  const std::vector<reify::CompilerEnvironment::InputModule>&
                      typescript_input_modules,
                  const std::function<void()>& on_initialization_complete,
                  QWidget* parent);

  using SaveAsReplyFunction = MonacoQtBridge::SaveAsReplyFunction;
  using QueryContentReplyFunction = MonacoQtBridge::QueryContentReplyFunction;

  bool initialized() const { return bridge_.initialized(); };

  void NewFile();
  void SaveAs(const QString& filepath,
              const SaveAsReplyFunction& on_completion);
  void Open(const QString& filepath, const QString& content);
  void QueryContent(const QueryContentReplyFunction& on_completion);

 private:
  MonacoQtBridge bridge_;
};

#endif  // WEB_INTERFACE_H_
