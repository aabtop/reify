#include "src/ide/mainwindow.h"

#include <QWebEngineView>

#include "src/ide/ui_mainwindow.h"

MainWindow::MainWindow(QWidget* parent)
    : QMainWindow(parent), ui(new Ui::MainWindow) {
  ui->setupUi(this);

  ui->editor->load(QUrl("qrc:/resources/monaco-wrapper/index.html"));
  ui->editor->show();
}

MainWindow::~MainWindow() { delete ui; }
