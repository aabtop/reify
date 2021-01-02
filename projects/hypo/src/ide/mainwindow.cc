#include "src/ide/mainwindow.h"

#include <qfiledialog.h>
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

  connect(monaco_interface_.get(),
          SIGNAL(OnSaveAsReply(const QString&, const QString&)), this,
          SLOT(SaveAsReply(const QString&, const QString&)));

  ui_->editor->load(QUrl("qrc:/src/ide/index.html"));
  ui_->editor->show();
}

MainWindow::~MainWindow() {}

void MainWindow::on_actionNew_triggered() {
  current_filepath_ = std::nullopt;
  emit monaco_interface_->NewFile();
}

void MainWindow::on_actionOpen_triggered() {
  std::cout << "on_actionOpen_triggered." << std::endl;
}

void MainWindow::on_actionSave_triggered() {
  if (current_filepath_) {
    emit monaco_interface_->SaveAs(QString(current_filepath_->c_str()));
  } else {
    on_actionSave_As_triggered();
  }
}

void MainWindow::on_actionSave_As_triggered() {
  QString filepath = QFileDialog::getSaveFileName(
      this, tr("Save As..."), "", tr("Typescript (*.ts);;All Files (*)"));

  if (filepath.isEmpty()) {
    return;
  }

  current_filepath_ = filepath.toStdString();
  emit monaco_interface_->SaveAs(filepath);
}

void MainWindow::on_actionExit_triggered() { close(); }

void MainWindow::on_actionAbout_triggered() {
  AboutDialog about_dialog;
  about_dialog.exec();
}

void MainWindow::SaveAsReply(const QString& filepath, const QString& content) {
  std::cout << "SaveAsReply: " << std::endl;
  std::cout << "  Filepath: " << filepath.toStdString() << std::endl;
  std::cout << "  Content: " << content.toStdString() << std::endl;
}
