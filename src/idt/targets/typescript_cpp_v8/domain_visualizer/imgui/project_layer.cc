#include "reify/typescript_cpp_v8/imgui/project_layer.h"

#include <fmt/format.h>

#include <filesystem>
#include <regex>

#include "imgui.h"
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
      domain_visualizer_(runtime_layer_->domain_visualizer()),
      self_work_queue_(self_work_queue) {
  if (initial_project_path) {
    LoadProject(*initial_project_path);
  }
}

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
        project_path, domain_visualizer_->GetTypeScriptModules());
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
  ImGui::PushStyleVar(ImGuiStyleVar_FramePadding, ImVec2(0, 8));
  if (ImGui::BeginMainMenuBar()) {
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

    ImGui::EndMainMenuBar();
  }
  ImGui::PopStyleVar();

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

}  // namespace imgui
}  // namespace typescript_cpp_v8
}  // namespace reify
