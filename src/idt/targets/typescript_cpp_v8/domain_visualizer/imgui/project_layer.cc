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

namespace {

utils::ErrorOr<std::unique_ptr<Project>> CreateDirectoryProjectFromPath(
    const std::filesystem::path& absolute_project_path,
    const std::vector<reify::CompilerEnvironment::InputModule>&
        typescript_input_modules) {
  if (!std::filesystem::is_directory(absolute_project_path)) {
    return utils::Error{fmt::format("Input path '{}' is not a directory.",
                                    absolute_project_path.string())};
  }

  std::unique_ptr<MountedHostFolderFilesystem> project_dir_filesystem(
      new MountedHostFolderFilesystem(absolute_project_path));

  auto get_sources = [source_file_regex = std::regex(
                          R"(.*\.ts)", std::regex_constants::ECMAScript,
                          std::regex_constants::optimize),
                      filesystem = project_dir_filesystem.get()] {
    std::set<std::string> source_files;
    for (auto& path : std::filesystem::recursive_directory_iterator(
             filesystem->host_root())) {
      std::string virtual_path = *filesystem->HostPathToVirtualPath(path);
      if (std::regex_search(virtual_path, source_file_regex)) {
        source_files.insert(virtual_path);
      }
    }
    return source_files;
  };

  return std::unique_ptr<Project>(new Project(
      absolute_project_path,
      std::unique_ptr<VirtualFilesystem>(project_dir_filesystem.release()),
      typescript_input_modules, get_sources));
}

utils::ErrorOr<std::unique_ptr<Project>> CreateFileProjectFromPath(
    const std::filesystem::path& absolute_input_source_file,
    const std::vector<reify::CompilerEnvironment::InputModule>&
        typescript_input_modules) {
  // Make or reference a virtual file system based on the current workspace.
  auto project_directory = absolute_input_source_file.parent_path();

  std::unique_ptr<MountedHostFolderFilesystem> virtual_filesystem(
      new MountedHostFolderFilesystem(project_directory));

  auto virtual_path =
      virtual_filesystem->HostPathToVirtualPath(absolute_input_source_file);

  if (!virtual_path) {
    return utils::Error{fmt::format(
        "Input file {} is not contained within the project root: {}",
        absolute_input_source_file.string(),
        virtual_filesystem->host_root().string())};
  }

  return std::unique_ptr<Project>(new Project(
      absolute_input_source_file,
      std::unique_ptr<VirtualFilesystem>(virtual_filesystem.release()),
      typescript_input_modules, [virtual_path_value = *virtual_path] {
        return std::set<std::string>{virtual_path_value};
      }));
}

}  // namespace

utils::ErrorOr<std::unique_ptr<Project>> CreateProjectFromPath(
    const std::filesystem::path& path,
    const std::vector<reify::CompilerEnvironment::InputModule>&
        typescript_input_modules) {
  if (!std::filesystem::exists(path)) {
    return utils::Error{
        fmt::format("Provided path {} does not exist.", path.string())};
  }
  auto absolute_path = std::filesystem::absolute(path);

  if (std::filesystem::is_directory(absolute_path)) {
    return CreateDirectoryProjectFromPath(absolute_path,
                                          typescript_input_modules);
  } else {
    return CreateFileProjectFromPath(absolute_path, typescript_input_modules);
  }
}

Project::Project(const std::filesystem::path& absolute_path,
                 std::unique_ptr<VirtualFilesystem> virtual_filesystem,
                 const std::vector<reify::CompilerEnvironment::InputModule>&
                     typescript_input_modules,
                 const std::function<std::set<std::string>()>& get_sources)
    : absolute_path_(absolute_path),
      virtual_filesystem_(std::move(virtual_filesystem)),
      get_sources_(get_sources),
      compiler_environment_(virtual_filesystem_.get(),
                            typescript_input_modules) {}

CompilerEnvironmentThreadSafe::MultiCompileFuture Project::RebuildProject() {
  return compiler_environment_.MultiCompile(get_sources_());
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

CompilerEnvironmentThreadSafe::CompileFuture
CompilerEnvironmentThreadSafe::Compile(const std::string& sources) {
  return compilation_thread_.EnqueueWithResult<CompileResults>(
      [this, sources] { return compiler_environment_->Compile(sources); });
}

CompilerEnvironmentThreadSafe::MultiCompileFuture
CompilerEnvironmentThreadSafe::MultiCompile(
    const std::set<std::string>& sources) {
  return compilation_thread_.EnqueueWithResult<MultiCompileResults>(
      [this, sources] {
        MultiCompileResults results;
        for (const auto& source : sources) {
          results[source] = compiler_environment_->Compile(source);
        }
        return results;
      });
}

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
      std::filesystem::absolute(project_path) == project_->absolute_path()) {
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
    auto error_or_project = CreateProjectFromPath(
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

  if (pending_compile_results_) {
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
