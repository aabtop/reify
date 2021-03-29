#ifndef WEB_INTERFACE_H_
#define WEB_INTERFACE_H_

#include <QFuture>
#include <QList>
#include <QObject>
#include <QPair>
#include <QWebChannel>
#include <QWebEnginePage>
#include <QWidget>

#include "reify/typescript_cpp_v8.h"

class WebInterface : public QObject {
  Q_OBJECT

 public:
  WebInterface(QWebEnginePage* page,
               const std::vector<reify::CompilerEnvironment::InputModule>&
                   typescript_input_modules,
               QWidget* parent = Q_NULLPTR);

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

  // private signals:
  void TypeScriptWrapperConstructor(const ModuleList& modules);
  // Signals emitted by the QWebEngine.
  void OnSaveAsReply(const QString& filepath,
                     const QString& content);  // Fired in response to SaveAs().
  // Signals emitted by the QWebEngine.
  void OnQueryContentReply(
      const QString& content);  // Fired in response to SaveAs().

 private:
  QWebChannel web_channel_;

  const std::vector<reify::CompilerEnvironment::InputModule>
      typescript_input_modules_;
};

#endif  // WEB_INTERFACE_H_
