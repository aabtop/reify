#ifndef MAINWINDOW_H
#define MAINWINDOW_H

#include <QMainWindow>
#include <QProgressBar>
#include <optional>
#include <string>
#include <thread>

#include "reify/typescript_cpp_v8/domain_visualizer.h"
#include "reify/typescript_cpp_v8/imgui/layer_stack.h"
#include "reify/typescript_cpp_v8/imgui/runtime_layer.h"
#include "reify/typescript_cpp_v8/imgui/status_layer.h"
#include "reify/window/window_stack.h"
#include "src/idt/targets/typescript_cpp_v8/ide/compilation.h"
#include "src/idt/targets/typescript_cpp_v8/ide/monaco_interface.h"

QT_BEGIN_NAMESPACE
namespace Ui {
class MainWindow;
}
QT_END_NAMESPACE

namespace reify {
namespace typescript_cpp_v8 {
namespace ide {

class MainWindow : public QMainWindow {
  Q_OBJECT

 public:
  MainWindow(const std::string& window_title,
             std::unique_ptr<DomainVisualizer> domain_visualizer,
             QWidget* parent = nullptr);
  ~MainWindow();

 protected:
  void closeEvent(QCloseEvent* event) override;

 private slots:
  void on_actionNew_triggered();
  void on_actionOpen_triggered();
  void on_actionSave_triggered();
  void on_actionSave_As_triggered();
  void on_actionExit_triggered();

  void on_actionPreview_triggered();

  void on_actionAbout_triggered();

 private:
  using Error = QString;
  struct SaveResults {
    QString filepath;
    QString contents;
  };
  template <typename T>
  using ErrorOr = std::variant<Error, T>;
  using SaveCompleteFunction = std::function<void(const ErrorOr<SaveResults>&)>;

  void ShowSaveErrorMessage(const QString& message);

  bool Save(const SaveCompleteFunction& save_complete_callback);
  bool SaveAs(const SaveCompleteFunction& save_complete_callback);
  void OnSaveAsComplete(const QString& filepath, const QString& content);
  void QueryContent(const MonacoInterface::QueryContentReplyFunction&
                        query_content_complete_callback);

  bool Compile(
      const std::function<void(std::shared_ptr<reify::CompiledModule>)>&
          compile_complete_callback);
  bool Build(const std::function<void()>& build_complete_callback);

  void FileDirtyStatusChange(bool is_dirty);
  void SaveIfDirtyCheck(const std::function<void()>& and_then);

  // Should be called whenever the internal state data changes in such a way
  // that it would cause the UI to appear different.
  void UpdateUiState();

  enum class PendingOperation {
    Initializing,
    Idle,
    QueryingContent,
    Saving,
    Compiling,
  };
  PendingOperation GetCurrentPendingOperation() const;
  bool HasPendingOperation() const {
    return GetCurrentPendingOperation() != PendingOperation::Idle;
  }

  std::unique_ptr<Ui::MainWindow> ui_;
  QString default_title_;

  std::unique_ptr<DomainVisualizer> domain_visualizer_;
  std::unique_ptr<QWidget> domain_visualizer_widget_;
  std::unique_ptr<MonacoInterface> monaco_interface_;

  std::unique_ptr<QProgressBar> progress_bar_;

  std::optional<SaveCompleteFunction> save_complete_callback_;
  std::optional<MonacoInterface::QueryContentReplyFunction>
      query_content_complete_callback_;

  std::optional<std::filesystem::path> current_filepath_;
  std::shared_ptr<reify::CompiledModule> most_recent_compilation_results_;

  std::optional<std::thread> project_operation_;

  imgui::StatusLayer visualizer_imgui_status_layer_;
  imgui::RuntimeLayer visualizer_imgui_runtime_layer_;
  imgui::LayerStack visualizer_imgui_stack_;
  window::WindowStack visualizer_window_;

  // Are the contents of the current file equivalent to what's saved on disk?
  bool current_file_is_dirty_ = false;
};

}  // namespace ide
}  // namespace typescript_cpp_v8
}  // namespace reify

#endif  // MAINWINDOW_H
