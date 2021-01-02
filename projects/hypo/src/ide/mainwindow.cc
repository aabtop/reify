#include "src/ide/mainwindow.h"

#include <qwebchannel.h>
#include <qwebengineview.h>

#include <iostream>

#include "src/ide/about_dialog.h"
#include "src/ide/ui_mainwindow.h"
#include "src/ide/web_interface.h"

MainWindow::MainWindow(QWidget* parent)
    : QMainWindow(parent), ui(new Ui::MainWindow) {
  ui->setupUi(this);

  QWebChannel* channel = new QWebChannel(ui->editor->page());
  ui->editor->page()->setWebChannel(channel);

  WebInterface* test_object = new WebInterface(this);
  channel->registerObject(QStringLiteral("test_object"), test_object);

  ui->editor->load(QUrl("qrc:/src/ide/index.html"));
  ui->editor->show();

  connect(ui->pushButton, SIGNAL(clicked()), test_object,
          SLOT(generateRandomData()));
}

MainWindow::~MainWindow() { delete ui; }

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
