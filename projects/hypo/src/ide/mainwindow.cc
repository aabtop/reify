#include "src/ide/mainwindow.h"

#include <qwebengineview.h>

#include "src/ide/ui_mainwindow.h"

MainWindow::MainWindow(QWidget* parent)
    : QMainWindow(parent), ui(new Ui::MainWindow) {
  ui->setupUi(this);

  ui->editor->load(QUrl("qrc:/src/ide/site/index.html"));
  ui->editor->show();
}

MainWindow::~MainWindow() { delete ui; }
