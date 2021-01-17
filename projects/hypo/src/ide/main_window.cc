#include "src/ide/main_window.h"

#include <qfiledialog.h>
#include <qmessagebox.h>
#include <qprogressbar.h>
#include <qwebengineview.h>

#include <fstream>
#include <sstream>

#include "reify/typescript_cpp_v8/hypo.h"
#include "src/ide/about_dialog.h"
#include "src/ide/ui_main_window.h"
#include "src/ide/web_interface.h"

MainWindow::MainWindow(QWidget* parent)
    : QMainWindow(parent), ui_(new Ui::MainWindow) {
  ui_->setupUi(this);
  default_title_ = windowTitle();

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

  progress_bar_.reset(new QProgressBar(this));
  statusBar()->addPermanentWidget(progress_bar_.get());
  progress_bar_->setMaximum(0);
  progress_bar_->setMinimum(0);
  progress_bar_->setValue(0);

  UpdateUiState();
}

MainWindow::~MainWindow() {}

void MainWindow::on_actionNew_triggered() {
  current_filepath_ = std::nullopt;
  OnCurrentFileChanged();
  emit monaco_interface_->NewFile();
}

void MainWindow::on_actionOpen_triggered() {
  QString filepath = QFileDialog::getOpenFileName(
      this, tr("Open"), "", tr("Typescript (*.ts);;All Files (*)"));

  if (filepath.isEmpty()) {
    return;
  }

  std::ifstream file(filepath.toStdString().c_str());
  std::stringstream content;
  content << file.rdbuf();

  if (file.fail()) {
    QMessageBox::warning(this, "Error opening file",
                         "Error while attempting to open file " + filepath);
    return;
  }

  current_filepath_ = filepath.toStdString();
  OnCurrentFileChanged();
  emit monaco_interface_->Open(filepath, QString(content.str().c_str()));
}

void MainWindow::on_actionSave_triggered() { Save(std::nullopt); }

void MainWindow::on_actionSave_As_triggered() { SaveAs(std::nullopt); }

void MainWindow::on_actionExit_triggered() { close(); }

void MainWindow::on_actionBuild_triggered() { Build(std::nullopt); }
void MainWindow::on_actionCompile_triggered() { Compile(std::nullopt); }

void MainWindow::on_actionAbout_triggered() {
  AboutDialog about_dialog;
  about_dialog.exec();
}

void MainWindow::SaveAsReply(const QString& filepath, const QString& content) {
  std::optional<std::function<void()>> save_complete_callback =
      std::move(save_complete_callback_);
  save_complete_callback_.reset();
  UpdateUiState();

  {
    std::ofstream file(filepath.toStdString().c_str());
    file << content.toStdString();

    if (file.fail()) {
      QMessageBox::warning(
          this, "Error saving file",
          "Error while attempting to save to file " + filepath);
      return;
    }

    // We need to ensure that the file is closed before we call any callbacks,
    // which may rely on the contents being flushed.
  }

  if (save_complete_callback) {
    (*save_complete_callback)();
  }
}

void MainWindow::OnCurrentFileChanged() {
  if (current_filepath_) {
    project_.emplace(*current_filepath_,
                     reify::typescript_cpp_v8::hypo::typescript_declarations());
  } else {
    project_.reset();
  }
  UpdateUiState();
}

bool MainWindow::Save(
    const std::optional<std::function<void()>>& save_complete_callback) {
  if (current_filepath_) {
    emit monaco_interface_->SaveAs(
        QString(current_filepath_->string().c_str()));
    save_complete_callback_ = save_complete_callback;
    return true;
  } else {
    return SaveAs(save_complete_callback);
  }
}

bool MainWindow::SaveAs(
    const std::optional<std::function<void()>>& save_complete_callback) {
  if (save_complete_callback_) {
    return false;
  }

  QString filepath = QFileDialog::getSaveFileName(
      this, tr("Save As..."), "", tr("Typescript (*.ts);;All Files (*)"));

  if (filepath.isEmpty()) {
    return false;
  }

  current_filepath_ = filepath.toStdString();
  OnCurrentFileChanged();
  emit monaco_interface_->SaveAs(filepath);

  save_complete_callback_ = save_complete_callback;

  UpdateUiState();
  return true;
}

bool MainWindow::Compile(
    const std::optional<std::function<void()>>& compile_complete_callback) {
  auto save_complete_callback = [this, compile_complete_callback]() {
    if (!current_filepath_) {
      QMessageBox::warning(this, "Error compiling",
                           "No file is marked as the current file.");
      return;
    }

    assert(!project_operation_);
    project_operation_.emplace([this, &project = project_,
                                current_filepath = *current_filepath_,
                                compile_complete_callback]() {
      auto result = project->CompileFile(current_filepath);
      QMetaObject::invokeMethod(
          this, [this, result, compile_complete_callback]() {
            project_operation_->join();
            project_operation_.reset();
            UpdateUiState();

            if (auto* error = std::get_if<Project::CompileError>(&result)) {
              QMessageBox::warning(this, "Error compiling",
                                   QString(error->c_str()));
              return;
            }

            auto compiled_module =
                std::get<std::shared_ptr<reify::CompiledModule>>(result);

            if (compile_complete_callback) {
              (*compile_complete_callback)();
            }
          });
    });
    UpdateUiState();
  };

  return Save(save_complete_callback);
}

bool MainWindow::Build(
    const std::optional<std::function<void()>>& build_complete_callback) {
  auto compile_complete_callback = [build_complete_callback]() {
    // TODO: Kick off a parallel thread to:
    //         1. Call the selected export to obtain the data.
    //         2. Pass data along into builder system.
    //         3. Pass builder output along into renderer system.
    if (build_complete_callback) {
      (*build_complete_callback)();
    }
  };

  return Compile(compile_complete_callback);
}

void MainWindow::UpdateUiState() {
  if (current_filepath_) {
    setWindowTitle(default_title_ + " - " +
                   QString(current_filepath_->string().c_str()));
  } else {
    setWindowTitle(default_title_);
  }

  if (save_complete_callback_) {
    progress_bar_->setVisible(true);
    statusBar()->showMessage(tr("Saving..."));
  } else if (project_operation_) {
    progress_bar_->setVisible(true);
    statusBar()->showMessage(tr("Compiling..."));
  } else {
    progress_bar_->setVisible(false);
    statusBar()->clearMessage();
  }
}
