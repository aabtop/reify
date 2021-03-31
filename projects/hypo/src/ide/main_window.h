#ifndef MAINWINDOW_H
#define MAINWINDOW_H

#include <QMainWindow>
#include <QProgressBar>
#include <optional>
#include <string>

#include "src/ide/compilation.h"
#include "src/ide/domain_visualizer.h"
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

  void on_actionBuild_triggered();
  void on_actionCompile_triggered();

  void on_actionAbout_triggered();

 private:
  bool Save(const std::optional<std::function<void()>>& save_complete_callback);
  bool SaveAs(
      const std::optional<std::function<void()>>& save_complete_callback);
  void OnSaveAsComplete(const QString& filepath, const QString& content);
  void QueryContent(
      const std::optional<std::function<void(const std::string&)>>&
          query_content_complete_callback);

  bool Compile(const std::optional<
               std::function<void(std::shared_ptr<reify::CompiledModule>)>>&
                   compile_complete_callback);
  bool Build(
      const std::optional<std::function<void()>>& build_complete_callback);

  // Should be called whenever the internal state data changes in such a way
  // that it would cause the UI to appear different.
  void UpdateUiState();

  enum class PendingOperation {
    Initializing,
    Idle,
    QueryingContent,
    Saving,
    Compiling,
    Building,
  };
  PendingOperation GetCurrentPendingOperation() const;
  bool HasPendingOperation() const {
    return GetCurrentPendingOperation() != PendingOperation::Idle;
  }

  std::unique_ptr<Ui::MainWindow> ui_;
  QString default_title_;

  std::unique_ptr<DomainVisualizer> domain_visualizer_;
  std::unique_ptr<WebInterface> monaco_interface_;

  std::unique_ptr<QProgressBar> progress_bar_;

  bool domain_build_active_ = false;

  std::optional<std::function<void()>> save_complete_callback_;
  std::optional<std::function<void(const std::string&)>>
      query_content_complete_callback_;

  std::optional<std::filesystem::path> current_filepath_;
  std::shared_ptr<reify::CompiledModule> most_recent_compilation_results_;

  std::optional<std::thread> project_operation_;
};
#endif  // MAINWINDOW_H
