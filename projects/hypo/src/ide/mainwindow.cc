#include "src/ide/mainwindow.h"

#include <qwebengineview.h>

#include <iostream>

#include "src/ide/about_dialog.h"
#include "src/ide/ui_mainwindow.h"
#include "src/ide/web_interface.h"

MainWindow::MainWindow(QWidget* parent)
    : QMainWindow(parent), ui_(new Ui::MainWindow) {
  ui_->setupUi(this);

  web_channel_.reset(new QWebChannel(ui_->editor->page()));
  ui_->editor->page()->setWebChannel(web_channel_.get());

  monaco_interface_.reset(new WebInterface(this));
  web_channel_->registerObject(QStringLiteral("monaco_qt_bridge"),
                               monaco_interface_.get());

  ui_->editor->load(QUrl("qrc:/src/ide/index.html"));
  ui_->editor->show();

  connect(ui_->pushButton, SIGNAL(clicked()), monaco_interface_.get(),
          SLOT(generateRandomData()));
}

MainWindow::~MainWindow() {}

void MainWindow::on_actionNew_triggered() {
  std::cout << "on_actionNew_triggered." << std::endl;
}
void MainWindow::on_actionOpen_triggered() {
  std::cout << "on_actionOpen_triggered." << std::endl;
}
void MainWindow::on_actionSave_triggered() {
  std::cout << "on_actionSave_triggered." << std::endl;
}
void MainWindow::on_actionSave_As_triggered() {
  std::cout << "on_actionSave_As_triggered." << std::endl;
}
void MainWindow::on_actionExit_triggered() {
  std::cout << "on_actionExit_triggered." << std::endl;
}
void MainWindow::on_actionAbout_triggered() {
  AboutDialog about_dialog;
  about_dialog.exec();
}
