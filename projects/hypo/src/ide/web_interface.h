#ifndef WEB_INTERFACE_H_
#define WEB_INTERFACE_H_

#include <QFuture>
#include <QObject>
#include <QWidget>

class WebInterface : public QObject {
  Q_OBJECT

 public:
  WebInterface(QWidget* parent = Q_NULLPTR);

 public slots:
  void SaveAsReply(const QString& filepath,
                   const QString& content);  // Called by the QWebEngine.

 signals:
  // Signals emitted by C++.
  void NewFile();
  void SaveAs(const QString& filepath);
  void Open(const QString& filepath, const QString& content);

  // Signals emitted by the QWebEngine.
  void OnSaveAsReply(const QString& filepath,
                     const QString& content);  // Fired in response to SaveAs().
};

#endif  // WEB_INTERFACE_H_
