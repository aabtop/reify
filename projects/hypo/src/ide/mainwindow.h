#ifndef MAINWINDOW_H
#define MAINWINDOW_H

#include <qmainwindow.h>
#include <qwebchannel.h>

#include <optional>
#include <string>

#include "src/ide/web_interface.h"

QT_BEGIN_NAMESPACE
namespace Ui {
class MainWindow;
}
QT_END_NAMESPACE

class MainWindow : public QMainWindow {
  Q_OBJECT

 public:
  MainWindow(QWidget* parent = nullptr);
  ~MainWindow();

 private slots:
  void on_actionNew_triggered();
  void on_actionOpen_triggered();
  void on_actionSave_triggered();
  void on_actionSave_As_triggered();
  void on_actionExit_triggered();

  void on_actionAbout_triggered();

  void SaveAsReply(const QString& filepath, const QString& content);

 private:
  std::unique_ptr<Ui::MainWindow> ui_;
  std::unique_ptr<QWebChannel> web_channel_;
  std::unique_ptr<WebInterface> monaco_interface_;

  std::optional<std::string> current_filepath_;
};
#endif  // MAINWINDOW_H
