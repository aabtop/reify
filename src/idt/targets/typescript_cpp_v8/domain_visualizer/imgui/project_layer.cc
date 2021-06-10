#include "reify/typescript_cpp_v8/imgui/project_layer.h"

#include <fmt/format.h>

#include "imgui.h"
#include "imgui_internal.h"
#include "platform_window/platform_window_key.h"
#include "reify/typescript_cpp_v8/imgui/utils.h"
#include "reify/typescript_cpp_v8/imgui/widgets.h"

namespace reify {
namespace typescript_cpp_v8 {
namespace imgui {

utils::ErrorOr<std::unique_ptr<Project>> CreateProjectFromPath(
    const std::filesystem::path& path,
    const std::vector<reify::CompilerEnvironment::InputModule>&
        typescript_input_modules) {
  if (!std::filesystem::exists(path) || std::filesystem::is_directory(path)) {
    return utils::Error{fmt::format(
        "Provided path {} either does not exist or is a directory, not a file.",
        path.string())};
  }

  // Make or reference a virtual file system based on the current workspace.
  auto absolute_input_source_file = std::filesystem::absolute(path);
  auto project_directory = absolute_input_source_file.parent_path();

  std::unique_ptr<MountedHostFolderFilesystem> virtual_filesystem(
      new MountedHostFolderFilesystem(project_directory));

  auto virtual_path =
      virtual_filesystem->HostPathToVirtualPath(absolute_input_source_file);

  if (!virtual_path) {
    return utils::Error{fmt::format(
        "Input file {} is not contained within the project root: {}",
        path.string(), virtual_filesystem->host_root().string())};
  }

  return std::unique_ptr<Project>(new Project(
      absolute_input_source_file,
      std::unique_ptr<VirtualFilesystem>(virtual_filesystem.release()),
      typescript_input_modules,
      [virtual_path_value = *virtual_path] { return virtual_path_value; }));
}

Project::Project(const std::filesystem::path& absolute_path,
                 std::unique_ptr<VirtualFilesystem> virtual_filesystem,
                 const std::vector<reify::CompilerEnvironment::InputModule>&
                     typescript_input_modules,
                 const std::function<std::string()>& get_sources)
    : absolute_path_(absolute_path),
      virtual_filesystem_(std::move(virtual_filesystem)),
      get_sources_(get_sources),
      compiler_environment_(virtual_filesystem_.get(),
                            typescript_input_modules) {}

CompilerEnvironmentThreadSafe::CompilationFuture Project::RebuildProject() {
  return compiler_environment_.Compile(get_sources_());
}

CompilerEnvironmentThreadSafe::CompilerEnvironmentThreadSafe(
    VirtualFilesystem* virtual_filesystem,
    const std::vector<reify::CompilerEnvironment::InputModule>&
        typescript_input_modules)
    : virtual_filesystem_(std::move(virtual_filesystem)),
      typescript_input_modules_(typescript_input_modules) {
  compilation_thread_.Enqueue([this] {
    compiler_environment_.emplace(virtual_filesystem_,
                                  &typescript_input_modules_);
  });
}

CompilerEnvironmentThreadSafe::~CompilerEnvironmentThreadSafe() {
  compilation_thread_.Enqueue([this] { compiler_environment_ = std::nullopt; });
}

CompilerEnvironmentThreadSafe::CompilationFuture
CompilerEnvironmentThreadSafe::Compile(const std::string& sources) {
  return compilation_thread_.EnqueueWithResult<CompilationResults>(
      [this, sources] { return compiler_environment_->Compile(sources); });
}

ProjectLayer::ProjectLayer(
    utils::WorkQueue* self_work_queue, StatusLayer* status_layer,
    RuntimeLayer* runtime_layer,
    const std::optional<std::filesystem::path>& initial_project_path)
    : self_work_queue_(self_work_queue),
      status_layer_(status_layer),
      runtime_layer_(runtime_layer),
      domain_visualizer_(runtime_layer_->domain_visualizer()) {
  if (initial_project_path) {
    LoadProject(*initial_project_path);
  }
}

void ProjectLayer::LoadProject(const std::filesystem::path& project_path) {
  if (pending_compilation_results_) {
    // Don't interrupt an existing load.
    return;
  }

  auto on_sources_compiled =
      [this](const CompilerEnvironmentThreadSafe::CompilationFuture::
                 CancelledOrSharedResult& x) {
        self_work_queue_.Enqueue([this, x] {
          pending_compilation_results_ = std::nullopt;
          if (std::holds_alternative<utils::CancelledFuture>(x)) {
            compilation_results_ = utils::Error{"Compilation cancelled."};
          } else {
            compilation_results_ = *std::get<1>(x);
          }
          if (auto module_compilation_result =
                  std::get_if<1>(&(*compilation_results_))) {
            if (auto compiled_module =
                    std::get_if<1>(module_compilation_result)) {
              runtime_layer_->SetCompiledModule(*compiled_module);
            }
          }
        });
      };

  if (project_ &&
      std::filesystem::absolute(project_path) == project_->absolute_path()) {
    // If an existing project exists with the same path, just re-build its
    // sources.
    pending_compilation_results_ =
        project_->RebuildProject().watch(on_sources_compiled);
  } else {
    if (project_) {
      // If the project path changes, clear out previous results to prepare
      // for loading a new project.
      runtime_layer_->SetCompiledModule(nullptr);
      compilation_results_ = std::nullopt;
      project_.reset();
    }

    // Load up a new project at the specified path.
    auto error_or_project = CreateProjectFromPath(
        project_path, domain_visualizer_->GetTypeScriptModules());
    if (auto error = std::get_if<0>(&error_or_project)) {
      compilation_results_ =
          utils::Error{fmt::format("Error creating project: {}", error->msg)};
    } else {
      // We've successfully created a new project, use it to start compiling.
      project_ = std::move(std::get<1>(error_or_project));
      pending_compilation_results_ =
          project_->RebuildProject().watch(on_sources_compiled);
    }
  }
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
                                             pending_compilation_results_);

      Action recompile_action(
          "Recompile", [this] { LoadProject(project_->absolute_path()); },
          !disable_if_no_project_loaded.condition(), kPlatformWindowKeyB);

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

  if (pending_compilation_results_) {
    if (!status_window_) {
      status_window_.emplace(status_layer_, [this] {
        Spinner("status compiling spinner", 10.0f, ImVec4{0.2, 0.6, 0.5, 1.0},
                ImVec4{0.1, 0.3, 0.2, 1.0}, 10, 2.5f);
        ImGui::SameLine();
        ImGui::Text("Compiling %s...",
                    project_->absolute_path().string().c_str());
      });
    }
  } else {
    status_window_ = std::nullopt;
  }
}

}  // namespace imgui
}  // namespace typescript_cpp_v8
}  // namespace reify
