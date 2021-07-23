#include "src/idt/targets/typescript_cpp_v8/ide/main_window.h"

#include <QCloseEvent>
#include <QFileDialog>
#include <QMessageBox>
#include <QProgressBar>
#include <QWebEngineView>
#include <fstream>
#include <sstream>

#include "reify/window/window_viewport.h"
#include "src/idt/targets/typescript_cpp_v8/ide/about_dialog.h"
#include "src/idt/targets/typescript_cpp_v8/ide/monaco_interface.h"
#include "src/idt/targets/typescript_cpp_v8/ide/reify_window_qt_widget.h"
#include "src/idt/targets/typescript_cpp_v8/ide/ui_main_window.h"

namespace reify {
namespace typescript_cpp_v8 {
namespace ide {

MainWindow::MainWindow(const std::string& window_title,
                       SymbolVisualizer* symbol_visualizer, QWidget* parent)
    : QMainWindow(parent),
      ui_(new Ui::MainWindow),
      default_title_(QString(window_title.c_str())),
      symbol_visualizer_(symbol_visualizer),
      visualizer_window_viewport_(symbol_visualizer_),
      visualizer_imgui_docking_layer_(ImGuiDir_Right, 0.2f),
      visualizer_imgui_docking_freespace_to_window_viewport_layer_(
          &visualizer_window_viewport_, &visualizer_imgui_docking_layer_),
      visualizer_imgui_status_layer_(&visualizer_imgui_docking_layer_),
      visualizer_imgui_runtime_layer_(
          [this](std::function<void()> x) {
            QMetaObject::invokeMethod(this, x);
          },
          &visualizer_imgui_docking_layer_, &visualizer_imgui_status_layer_,
          symbol_visualizer_),
      visualizer_imgui_stack_({
          [docking_layer = &visualizer_imgui_docking_layer_]() {
            docking_layer->ExecuteImGuiCommands();
          },
          [status_layer = &visualizer_imgui_status_layer_]() {
            status_layer->ExecuteImGuiCommands();
          },
          [runtime_layer = &visualizer_imgui_runtime_layer_]() {
            runtime_layer->ExecuteImGuiCommands();
          },
          [docking_freespace_to_window_viewport_layer =
               &visualizer_imgui_docking_freespace_to_window_viewport_layer_]() {
            docking_freespace_to_window_viewport_layer->ExecuteImGuiCommands();
          },
      }),
      visualizer_window_(
          {&visualizer_window_viewport_, &visualizer_imgui_stack_}) {
  ui_->setupUi(this);

  ui_->visualizer->setAutoFillBackground(false);
  ui_->visualizer->setStyleSheet("background-color:transparent;");

  symbol_visualizer_widget_ =
      MakeReifyWindowWidget(&visualizer_window_, ui_->visualizer);

  monaco_interface_.reset(new MonacoInterface(
      ui_->editor->page(), symbol_visualizer_->GetTypeScriptModules(),
      [this]() { UpdateUiState(); },
      [this](bool is_dirty) { FileDirtyStatusChange(is_dirty); }, this));

  ui_->editor->load(
      QUrl("qrc:/src/idt/targets/typescript_cpp_v8/ide/index.html"));

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

void MainWindow::SaveIfDirtyCheck(const std::function<void()>& and_then) {
  if (!current_file_is_dirty_) {
    and_then();
    return;
  }

  QMessageBox message_box(
      QMessageBox::Question, "Save?", "The document has been modified.",
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
      if (!Save([this, and_then](const ErrorOr<SaveResults>& maybe_results) {
            if (auto* error = std::get_if<0>(&maybe_results)) {
              ShowSaveErrorMessage(*error);
            } else {
              and_then();
            }
          })) {
        ShowSaveErrorMessage("Unknown error while attempting to save.");
        return;
      }
      break;
    case QMessageBox::Discard:
      // "Don't Save" was clicked.
      and_then();
      break;
    case QMessageBox::Cancel:
      break;
    default:
      assert(false);
      break;
  }
}

void MainWindow::closeEvent(QCloseEvent* event) {
  event->ignore();
  if (HasPendingOperation()) {
    return;
  }

  SaveIfDirtyCheck([]() { QApplication::quit(); });
}

void MainWindow::on_actionNew_triggered() {
  SaveIfDirtyCheck([this]() {
    file_project_.reset();
    current_file_is_dirty_ = false;
    UpdateUiState();
    monaco_interface_->NewFile();
  });
}

void MainWindow::on_actionOpen_triggered() {
  SaveIfDirtyCheck([this]() {
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

    file_project_ = std::make_shared<FileProject>(CreateFileProject(
        filepath.toStdString(), symbol_visualizer_->GetTypeScriptModules()));

    current_file_is_dirty_ = false;
    UpdateUiState();
    monaco_interface_->Open(filepath, QString(content.str().c_str()));
  });
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

  if (file_project_) {
    save_complete_callback_ = save_complete_callback;
    monaco_interface_->SaveAs(
        QString(file_project_->filepath.string().c_str()),
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

  file_project_ = std::make_shared<FileProject>(CreateFileProject(
      filepath.toStdString(), symbol_visualizer_->GetTypeScriptModules()));
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
    const std::function<void(std::shared_ptr<CompiledModule>)>&
        compile_complete_callback) {
  if (HasPendingOperation()) {
    return false;
  }

  auto query_content_complete_callback = [this, compile_complete_callback](
                                             const QString& content) {
    assert(!project_operation_);
    project_operation_.emplace([this, content = content.toStdString(),
                                compile_complete_callback]() {
      auto result = [typescript_modules =
                         symbol_visualizer_->GetTypeScriptModules(),
                     &content, file_project = file_project_]() {
        if (file_project) {
          return std::get<1>(file_project->project.RebuildProject()
                                 .wait_and_get_results())
              .begin()
              ->second;
        } else {
          return CompileContents(typescript_modules, content);
        }
      }();

      QMetaObject::invokeMethod(this, [this, result,
                                       compile_complete_callback]() {
        project_operation_->join();
        project_operation_.reset();

        if (auto* error = std::get_if<CompileError>(&result)) {
          UpdateUiState();
          QMessageBox::warning(this, "Error compiling",
                               QString(CompileErrorToString(*error).c_str()));
          return;
        }

        most_recent_compilation_results_ =
            std::get<std::shared_ptr<CompiledModule>>(result);

        UpdateUiState();

        compile_complete_callback(most_recent_compilation_results_);
      });
    });
    UpdateUiState();
  };

  if (file_project_) {
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
          std::shared_ptr<CompiledModule> compiled_module) {
        visualizer_imgui_runtime_layer_.SetCompileResults(
            {{*VirtualFilesystem::AbsolutePath::FromComponents(
                  {file_project_ ? file_project_->filepath.filename().string()
                                 : "untitled"}),
              compiled_module}});
        UpdateUiState();

        bool previewable_symbols = false;
        for (const auto& symbol : compiled_module->exported_symbols()) {
          if (symbol_visualizer_->CanPreviewSymbol(symbol)) {
            previewable_symbols = true;
          }
        }
        if (!previewable_symbols) {
          QMessageBox::warning(
              this, "No symbols available",
              QString("You must export symbols of the correct type."));
        }

        build_complete_callback();
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
  } else {
    return PendingOperation::Idle;
  }
}

void MainWindow::UpdateUiState() {
  QString title = default_title_ + " - ";
  if (file_project_) {
    title += QString(file_project_->filepath.string().c_str());
  } else {
    title += "untitled";
  }
  if (current_file_is_dirty_) {
    title += " *";
  }
  setWindowTitle(title);

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
  }
}

}  // namespace ide
}  // namespace typescript_cpp_v8
}  // namespace reify