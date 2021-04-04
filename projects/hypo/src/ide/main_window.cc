#include "src/ide/main_window.h"

#include <QCloseEvent>
#include <QFileDialog>
#include <QMessageBox>
#include <QProgressBar>
#include <QWebEngineView>
#include <fstream>
#include <sstream>

#include "src/ide/about_dialog.h"
#include "src/ide/domain_visualizer_qt.h"
#include "src/ide/monaco_interface.h"
#include "src/ide/ui_main_window.h"

MainWindow::MainWindow(QWidget* parent)
    : QMainWindow(parent), ui_(new Ui::MainWindow) {
  ui_->setupUi(this);
  default_title_ = windowTitle();

  ui_->visualizer->setAutoFillBackground(false);
  ui_->visualizer->setStyleSheet("background-color:transparent;");
  domain_visualizer_ = CreateDefaultQtWidgetDomainVisualizer(ui_->visualizer);

  monaco_interface_.reset(new MonacoInterface(
      ui_->editor->page(), domain_visualizer_->GetTypeScriptModules(),
      [this]() { UpdateUiState(); },
      [this](bool is_dirty) { FileDirtyStatusChange(is_dirty); }, this));

  ui_->editor->load(QUrl("qrc:/src/ide/index.html"));

  progress_bar_.reset(new QProgressBar(this));
  progress_bar_->setMaximum(0);
  progress_bar_->setMinimum(0);
  progress_bar_->setValue(0);
  progress_bar_->setTextVisible(false);
  statusBar()->addPermanentWidget(progress_bar_.get());
  statusBar()->setContentsMargins(0, 0, 0, 2);
  UpdateUiState();

  int halfSplitterParentWidth =
      ui_->horizontalSplitter->parentWidget()->width() / 2;
  ui_->horizontalSplitter->setSizes(
      {halfSplitterParentWidth, 2 * halfSplitterParentWidth});

  ui_->editor->page()->setBackgroundColor(Qt::transparent);

  ui_->editor->show();
}

MainWindow::~MainWindow() {}

void MainWindow::ShowSaveErrorMessage(const QString& message) {
  QMessageBox::warning(this, "Error saving", message);
}

void MainWindow::closeEvent(QCloseEvent* event) {
  if (HasPendingOperation()) {
    event->ignore();
    return;
  }

  if (current_file_is_dirty_) {
    QMessageBox message_box(
        QMessageBox::Question, "Save before exiting?",
        "The document has been modified.",
        QMessageBox::Save | QMessageBox::Discard | QMessageBox::Cancel, this,
        Qt::Dialog | Qt::Popup | Qt::FramelessWindowHint);
    message_box.setModal(true);
    message_box.setInformativeText("Do you want to save your changes?");
    message_box.setDefaultButton(QMessageBox::Cancel);
    int ret = message_box.exec();

    switch (ret) {
      case QMessageBox::Save:
        // We uncondtionally ignore because if the save succeeds, we will
        // actually exit in the callback.
        event->ignore();
        if (!Save([this](const ErrorOr<SaveResults>& maybe_results) {
              if (auto* error = std::get_if<0>(&maybe_results)) {
                ShowSaveErrorMessage(*error);
              } else {
                QApplication::quit();
              }
            })) {
          ShowSaveErrorMessage("Unknown error while attempting to save.");
          return;
        }
        break;
      case QMessageBox::Discard:
        // Don't Save was clicked
        event->accept();
        break;
      case QMessageBox::Cancel:
        event->ignore();
        break;
      default:
        assert(false);
        break;
    }

  } else {
    event->accept();
  }
}

void MainWindow::on_actionNew_triggered() {
  current_filepath_ = std::nullopt;
  current_file_is_dirty_ = false;
  UpdateUiState();
  monaco_interface_->NewFile();
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
  current_file_is_dirty_ = false;
  UpdateUiState();
  monaco_interface_->Open(filepath, QString(content.str().c_str()));
}

void MainWindow::on_actionSave_triggered() {
  Save([](const auto&) {});
}

void MainWindow::on_actionSave_As_triggered() {
  SaveAs([](const auto&) {});
}

void MainWindow::on_actionExit_triggered() { close(); }

void MainWindow::on_actionPreview_triggered() {
  Build([]() {});
}

void MainWindow::on_actionAbout_triggered() {
  AboutDialog about_dialog;
  about_dialog.exec();
}

bool MainWindow::Save(const SaveCompleteFunction& save_complete_callback) {
  if (HasPendingOperation()) {
    return false;
  }

  if (current_filepath_) {
    save_complete_callback_ = save_complete_callback;
    monaco_interface_->SaveAs(
        QString(current_filepath_->string().c_str()),
        [this](const QString& filepath, const QString& content) {
          OnSaveAsComplete(filepath, content);
        });
    UpdateUiState();
    return true;
  } else {
    return SaveAs(save_complete_callback);
  }
}

bool MainWindow::SaveAs(const SaveCompleteFunction& save_complete_callback) {
  if (HasPendingOperation()) {
    return false;
  }

  QString filepath = QFileDialog::getSaveFileName(
      this, tr("Save As..."), "", tr("Typescript (*.ts);;All Files (*)"));

  if (filepath.isEmpty()) {
    return false;
  }

  current_filepath_ = filepath.toStdString();
  save_complete_callback_ = save_complete_callback;

  monaco_interface_->SaveAs(
      filepath, [this](const QString& filepath, const QString& content) {
        OnSaveAsComplete(filepath, content);
      });

  UpdateUiState();
  return true;
}

void MainWindow::OnSaveAsComplete(const QString& filepath,
                                  const QString& content) {
  auto save_complete_callback = std::move(save_complete_callback_);
  save_complete_callback_.reset();
  UpdateUiState();

  {
    std::ofstream file(filepath.toStdString().c_str());
    file << content.toStdString();

    if (file.fail()) {
      if (save_complete_callback) {
        (*save_complete_callback)(
            Error("Error while attempting to save to file " + filepath));
      }

      return;
    }

    // We need to ensure that the file is closed before we call any
    // callbacks, which may rely on the contents being flushed.
  }

  current_file_is_dirty_ = false;
  UpdateUiState();

  if (save_complete_callback) {
    (*save_complete_callback)(SaveResults{filepath, content});
  }
}

void MainWindow::QueryContent(const MonacoInterface::QueryContentReplyFunction&
                                  query_content_complete_callback) {
  assert(!HasPendingOperation());
  query_content_complete_callback_ = query_content_complete_callback;
  monaco_interface_->QueryContent([this](const QString& content) {
    auto query_content_complete_callback =
        std::move(query_content_complete_callback_);
    query_content_complete_callback_.reset();

    if (query_content_complete_callback) {
      (*query_content_complete_callback)(content);
    }

    UpdateUiState();
  });

  UpdateUiState();
}

bool MainWindow::Compile(
    const std::function<void(std::shared_ptr<reify::CompiledModule>)>&
        compile_complete_callback) {
  if (HasPendingOperation()) {
    return false;
  }

  auto query_content_complete_callback =
      [this, compile_complete_callback](const QString& content) {
        assert(!project_operation_);
        project_operation_.emplace([this, content = content.toStdString(),
                                    compile_complete_callback]() {
          auto result = [typescript_modules =
                             domain_visualizer_->GetTypeScriptModules(),
                         &content, current_filepath = current_filepath_]() {
            if (current_filepath) {
              return CompileFile(typescript_modules, *current_filepath);
            } else {
              return CompileContents(typescript_modules, content);
            }
          }();

          QMetaObject::invokeMethod(
              this, [this, result, compile_complete_callback]() {
                project_operation_->join();
                project_operation_.reset();

                if (auto* error = std::get_if<CompileError>(&result)) {
                  UpdateUiState();
                  QMessageBox::warning(this, "Error compiling",
                                       QString(error->c_str()));
                  return;
                }

                most_recent_compilation_results_ =
                    std::get<std::shared_ptr<reify::CompiledModule>>(result);

                UpdateUiState();

                compile_complete_callback(most_recent_compilation_results_);
              });
        });
        UpdateUiState();
      };

  if (current_filepath_) {
    return Save([this, query_content_complete_callback](
                    const ErrorOr<SaveResults>& maybe_results) {
      if (auto* error = std::get_if<0>(&maybe_results)) {
        ShowSaveErrorMessage(*error);
        return;
      }
      query_content_complete_callback(std::get<1>(maybe_results).contents);
    });
  } else {
    QueryContent(query_content_complete_callback);
    return true;
  }
}

bool MainWindow::Build(const std::function<void()>& build_complete_callback) {
  if (HasPendingOperation()) {
    return false;
  }

  auto compile_complete_callback =
      [this, build_complete_callback](
          std::shared_ptr<reify::CompiledModule> compiled_module) {
        if (ui_->comboBox->currentIndex() == -1) {
          if (ui_->comboBox->count() == 0) {
            QMessageBox::warning(
                this, "No symbols available",
                QString("You must export symbols of the correct type."));
            return;
          } else if (ui_->comboBox->count() > 0) {
            ui_->comboBox->setCurrentIndex(0);
          }
        }

        std::string symbol_name = ui_->comboBox->currentText().toStdString();

        domain_build_active_ = true;
        UpdateUiState();

        domain_visualizer_->ConsumeSymbol(
            compiled_module, *compiled_module->GetExportedSymbol(symbol_name),
            [this, build_complete_callback](
                std::optional<DomainVisualizer::ConsumeError>&& error) {
              QMetaObject::invokeMethod(this, [this, error = std::move(error),
                                               build_complete_callback]() {
                domain_build_active_ = false;
                UpdateUiState();

                if (error) {
                  QMessageBox::warning(this, "Error building symbol",
                                       QString(error->c_str()));
                  return;
                }

                build_complete_callback();
              });
            });
      };
  return Compile(compile_complete_callback);
}

void MainWindow::FileDirtyStatusChange(bool is_dirty) {
  current_file_is_dirty_ = is_dirty;
  UpdateUiState();
}

MainWindow::PendingOperation MainWindow::GetCurrentPendingOperation() const {
  if (!monaco_interface_->initialized()) {
    return PendingOperation::Initializing;
  } else if (save_complete_callback_) {
    return PendingOperation::Saving;
  } else if (query_content_complete_callback_) {
    return PendingOperation::QueryingContent;
  } else if (project_operation_) {
    return PendingOperation::Compiling;
  } else if (domain_build_active_) {
    return PendingOperation::Building;
  } else {
    return PendingOperation::Idle;
  }
}

void MainWindow::UpdateUiState() {
  QString title = default_title_ + " - ";
  if (current_filepath_) {
    title += QString(current_filepath_->string().c_str());
  } else {
    title += "untitled";
  }
  if (current_file_is_dirty_) {
    title += " *";
  }
  setWindowTitle(title);

  if (most_recent_compilation_results_) {
    std::optional<QString> previous_combo_box_text;
    if (ui_->comboBox->currentIndex() != -1) {
      previous_combo_box_text = ui_->comboBox->currentText();
    }

    ui_->comboBox->clear();
    if (most_recent_compilation_results_) {
      for (const auto& symbol :
           most_recent_compilation_results_->exported_symbols()) {
        if (domain_visualizer_->CanConsumeSymbol(symbol)) {
          ui_->comboBox->addItem(QString(symbol.name.c_str()));
        }
      }
    }

    if (!previous_combo_box_text) {
      // If nothing was selected before the compile, maintain this.
      ui_->comboBox->setCurrentIndex(-1);
    } else {
      ui_->comboBox->setCurrentIndex(
          ui_->comboBox->findText(*previous_combo_box_text));
    }
  }

  switch (GetCurrentPendingOperation()) {
    case PendingOperation::Initializing: {
      progress_bar_->setVisible(false);
      statusBar()->showMessage(tr("Initializing..."));
    } break;
    case PendingOperation::Idle: {
      progress_bar_->setVisible(false);
      statusBar()->clearMessage();
    } break;
    case PendingOperation::QueryingContent: {
      progress_bar_->setVisible(true);
      statusBar()->showMessage(tr("Querying text content..."));
    } break;
    case PendingOperation::Saving: {
      progress_bar_->setVisible(true);
      statusBar()->showMessage(tr("Saving..."));
    } break;
    case PendingOperation::Compiling: {
      progress_bar_->setVisible(true);
      statusBar()->showMessage(tr("Compiling..."));
    } break;
    case PendingOperation::Building: {
      progress_bar_->setVisible(true);
      statusBar()->showMessage(tr("Building..."));
    } break;
  }
}
