#include "reify/typescript_cpp_v8/imgui/project_layer.h"

#include <fmt/format.h>

#include <filesystem>
#include <regex>

// clang-format off
#include "imgui.h"
#include "imfilebrowser.h"
// clang-format on
#include "imgui_internal.h"
#include "platform_window/platform_window_key.h"
#include "reify/typescript_cpp_v8/imgui/utils.h"
#include "reify/typescript_cpp_v8/imgui/widgets.h"

namespace reify {
namespace typescript_cpp_v8 {
namespace imgui {

ProjectLayer::ProjectLayer(
    utils::WorkQueue* self_work_queue, StatusLayer* status_layer,
    RuntimeLayer* runtime_layer,
    const std::optional<std::filesystem::path>& initial_project_path)
    : status_layer_(status_layer),
      runtime_layer_(runtime_layer),
      symbol_visualizer_(runtime_layer_->symbol_visualizer()),
      self_work_queue_(self_work_queue) {
  if (initial_project_path) {
    LoadProject(*initial_project_path);
  }
}

ProjectLayer::~ProjectLayer() {}

void ProjectLayer::LoadProject(const std::filesystem::path& project_path) {
  if (pending_compile_results_) {
    // Don't interrupt an existing load.
    std::cerr << "An existing compilation is in progress." << std::endl;
    return;
  }

  auto on_sources_compiled =
      [this](const CompilerEnvironmentThreadSafe::MultiCompileFuture::
                 CancelledOrSharedResult& x) {
        self_work_queue_.Enqueue([this, x] {
          pending_compile_results_ = std::nullopt;
          if (std::holds_alternative<utils::CancelledFuture>(x)) {
            compile_results_ = std::nullopt;
          } else {
            compile_results_ = *std::get<1>(x);
            runtime_layer_->SetCompileResults(*compile_results_);
          }
        });
      };

  if (project_ &&
      std::filesystem::absolute(project_path) ==
          project_->host_filesystem_project.virtual_filesystem->host_root()) {
    // If an existing project exists with the same path, just re-build its
    // sources.
    pending_compile_results_ =
        project_->RebuildProject().watch(on_sources_compiled);
  } else {
    if (project_) {
      // If the project path changes, clear out previous results to prepare
      // for loading a new project.
      runtime_layer_->SetCompileResults({});
      compile_results_ = std::nullopt;
      project_.reset();
    }

    // Load up a new project at the specified path.
    auto error_or_project = CreateProjectWithDefaultBuildFilesGetterFromPath(
        project_path, symbol_visualizer_->GetTypeScriptModules());
    if (auto error = std::get_if<0>(&error_or_project)) {
      compile_results_ = std::nullopt;
      std::cerr << "Error creating project: " << error->msg << std::endl;
      return;
    } else {
      // We've successfully created a new project, use it to start compiling.
      project_ = std::move(std::get<1>(error_or_project));
      pending_compile_results_ =
          project_->RebuildProject().watch(on_sources_compiled);
    }
  }
  return;
}

std::optional<reify::utils::Error> ProjectLayer::CreateProjectDirectory(
    const std::filesystem::path& project_path) {
  if (std::filesystem::exists(project_path)) {
    return reify::utils::Error{
        fmt::format("Path {} already exists! Choose a directory that doesn't "
                    "already exist since some project files like "
                    "`tsconfig.json` will be written into it.",
                    project_path.string())};
  }

  auto maybe_error = CompilerEnvironment::CreateWorkspaceDirectory(
      project_path, symbol_visualizer_->GetTypeScriptModules());
  if (maybe_error) {
    return *maybe_error;
  }

  // If the creation was successful, load the directory as a project.
  LoadProject(std::filesystem::canonical(project_path));
  return std::nullopt;
}

namespace {

// Helper class for setting up menu items with shortcuts.
class Action {
 public:
  Action(const std::string& menu_label, const std::function<void()>& run,
         bool enabled, PlatformWindowKey shortcut = kPlatformWindowKeyUnknown)
      : menu_label_(menu_label),
        enabled_(enabled),
        shortcut_(shortcut),
        run_(run) {}

  void MenuItem(bool enabled) {
    std::string shortcut_string;
    if (shortcut_ != kPlatformWindowKeyUnknown) {
      shortcut_string =
          std::string("CTRL+") +
          static_cast<char>('A' + (shortcut_ - kPlatformWindowKeyA));
    }

    bool result = ImGui::MenuItem(
        menu_label_.c_str(),
        shortcut_string.empty() ? nullptr : shortcut_string.c_str());

    if (result) {
      run_();
      menu_item_called_ = true;
    }
  }
  ~Action() {
    if (!menu_item_called_ && enabled_ &&
        ImGui::GetIO().KeyMods & ImGuiKeyModFlags_Ctrl &&
        ImGui::IsKeyPressed(shortcut_, false)) {
      run_();
    }
  }

 private:
  const std::string menu_label_;
  const bool enabled_;
  const PlatformWindowKey shortcut_;
  const std::function<void()> run_;

  bool menu_item_called_ = false;
};

}  // namespace
void ProjectLayer::ExecuteImGuiCommands() {
  if (show_modal_message_) {
  }

  ImGui::PushStyleVar(ImGuiStyleVar_FramePadding, ImVec2(0, 8));
  if (ImGui::BeginMainMenuBar()) {
    Action create_project_directory_action(
        "Create project directory",
        [this] {
          file_dialog_ =
              FileDialog{FileDialog::Action::CreateProjectDirectory,
                         std::make_unique<ImGui::FileBrowser>(
                             ImGuiFileBrowserFlags_CloseOnEsc |
                             ImGuiFileBrowserFlags_EnterNewFilename)};
          file_dialog_->file_browser->SetTitle("Create project directory");
          file_dialog_->file_browser->Open();
        },
        !file_dialog_);
    Action open_file_action(
        "Open file",
        [this] {
          file_dialog_ = FileDialog{FileDialog::Action::OpenFile,
                                    std::make_unique<ImGui::FileBrowser>(
                                        ImGuiFileBrowserFlags_CloseOnEsc)};
          file_dialog_->file_browser->SetTitle("Open file");
          file_dialog_->file_browser->SetTypeFilters({".ts"});
          file_dialog_->file_browser->Open();
        },
        !file_dialog_, kPlatformWindowKeyO);
    Action open_dir_action(
        "Open directory",
        [this] {
          file_dialog_ = FileDialog{FileDialog::Action::OpenDirectory,
                                    std::make_unique<ImGui::FileBrowser>(
                                        ImGuiFileBrowserFlags_CloseOnEsc |
                                        ImGuiFileBrowserFlags_SelectDirectory)};
          file_dialog_->file_browser->SetTitle("Open directory");
          file_dialog_->file_browser->Open();
        },
        !file_dialog_);

    if (ImGui::BeginMenu("File")) {
      create_project_directory_action.MenuItem(true);
      open_file_action.MenuItem(true);
      open_dir_action.MenuItem(true);
      ImGui::EndMenu();
    }

    {
      DisableIf disable_if_no_project_loaded(!project_ ||
                                             pending_compile_results_);

      Action recompile_action(
          "Recompile",
          [this] {
            LoadProject(project_->host_filesystem_project.virtual_filesystem
                            ->host_root());
          },
          !disable_if_no_project_loaded.condition(), kPlatformWindowKeyR);

      if (ImGui::BeginMenu("Project")) {
        recompile_action.MenuItem(!disable_if_no_project_loaded.condition());
        ImGui::EndMenu();
      }
    }

    if (ImGui::BeginMenu("Help")) {
      ImGui::MenuItem("About");
      ImGui::EndMenu();
    }

    // Render text that shows the path of the currently loaded project in the
    // menu bar center.
    {
      std::string project_path_text = fmt::format("({})", [this] {
        if (project_) {
          return project_->host_filesystem_project.virtual_filesystem
              ->host_root()
              .string();
        } else {
          return std::string("no project loaded");
        }
      }());
      float text_width = ImGui::CalcTextSize(project_path_text.c_str()).x;
      float current_position = ImGui::GetCurrentWindow()->DC.CursorPos.x;
      float remaining_space = ImGui::GetWindowSize().x - current_position;

      if (text_width < remaining_space) {
        ImGui::PushStyleVar(ImGuiStyleVar_Alpha,
                            ImGui::GetStyle().Alpha * 0.5f);
        ImGui::SameLine(current_position + (remaining_space - text_width) / 2);
        ImGui::Text("%s", project_path_text.c_str());
        ImGui::PopStyleVar();
      }
    }

    ImGui::EndMainMenuBar();
  }
  ImGui::PopStyleVar();

  if (file_dialog_) {
    file_dialog_->file_browser->Display();
    if (file_dialog_->file_browser->HasSelected()) {
      switch (file_dialog_->action) {
        case FileDialog::Action::OpenFile:
        case FileDialog::Action::OpenDirectory: {
          LoadProject(std::filesystem::canonical(
              file_dialog_->file_browser->GetSelected()));
        } break;
        case FileDialog::Action::CreateProjectDirectory: {
          auto maybe_error =
              CreateProjectDirectory(file_dialog_->file_browser->GetSelected());
          if (maybe_error) {
            show_modal_message_ =
                fmt::format("Error attempting to create project directory: {}",
                            maybe_error->msg);
            ImGui::OpenPopup("Error creating project directory");
          }
        } break;
      }
      file_dialog_->file_browser->Close();

      if (!file_dialog_->file_browser->IsOpened()) {
        file_dialog_ = std::nullopt;
      }
    }

    if (pending_compile_results_) {
      if (!status_window_) {
        status_window_.emplace(status_layer_, [this] {
          Spinner("status compiling spinner", 10.0f, ImVec4{0.2, 0.6, 0.5, 1.0},
                  ImVec4{0.1, 0.3, 0.2, 1.0}, 10, 2.5f);
          ImGui::SameLine();
          ImGui::Text(
              "Compiling %s...",
              project_->host_filesystem_project.virtual_filesystem->host_root()
                  .string()
                  .c_str());
        });
      }
    } else {
      status_window_ = std::nullopt;
    }
  }

  if (ImGui::BeginPopupModal("Error creating project directory", NULL,
                             ImGuiWindowFlags_AlwaysAutoResize)) {
    ImGui::TextUnformatted(show_modal_message_->c_str());
    ImGui::Separator();
    if (ImGui::Button("OK")) {
      ImGui::CloseCurrentPopup();
    }
    ImGui::EndPopup();
  }
}

}  // namespace imgui
}  // namespace typescript_cpp_v8
}  // namespace reify
