#include "reify/typescript_cpp_v8/imgui/project_layer.h"

#include <fmt/format.h>

#include "imgui.h"
#include "imgui_internal.h"
#include "reify/typescript_cpp_v8/imgui/utils.h"
#include "reify/typescript_cpp_v8/imgui/widgets.h"

namespace reify {
namespace typescript_cpp_v8 {
namespace imgui {

namespace {
utils::ErrorOr<std::shared_ptr<reify::CompiledModule>> CompileVirtualFile(
    const std::vector<reify::CompilerEnvironment::InputModule>&
        typescript_input_modules,
    const std::string& virtual_filepath, reify::VirtualFilesystem* vfs) {
  // V8 doesn't like being initialized on one thread and then used on another,
  // so we just recreate the compiler environment each time.  This means we
  // need to reload the TypeScript compiler every time we want to compile
  // something, which kind of sucks.  The output of the Compile() call is
  // completely independent of the compiler environment, so at least we're
  // safe there.
  auto result = [&]() {
    reify::CompilerEnvironment compile_env(vfs, &typescript_input_modules);
    return compile_env.Compile(virtual_filepath);
  }();

  if (auto error = std::get_if<0>(&result)) {
    return utils::Error{fmt::format("{}:{}:{}: error: {}", error->path,
                                    error->line + 1, error->column + 1,
                                    error->message)};
  }

  return std::get<1>(result);
}

utils::ErrorOr<std::shared_ptr<reify::CompiledModule>> CompileFile(
    const std::vector<reify::CompilerEnvironment::InputModule>&
        typescript_input_modules,
    const std::filesystem::path& filepath) {
  if (!std::filesystem::exists(filepath) ||
      std::filesystem::is_directory(filepath)) {
    return utils::Error{fmt::format(
        "Provided path {} either does not exist or is a directory, not a file.",
        filepath.string())};
  }

  // Make or reference a virtual file system based on the current workspace.
  auto absolute_input_source_file = std::filesystem::absolute(filepath);
  auto project_directory = absolute_input_source_file.parent_path();

  reify::MountedHostFolderFilesystem vfs(project_directory);

  auto virtual_path = vfs.HostPathToVirtualPath(absolute_input_source_file);

  if (!virtual_path) {
    return utils::Error{fmt::format(
        "Input file {} is not contained within the project root: {}",
        filepath.string(), vfs.host_root().string())};
  }

  return CompileVirtualFile(typescript_input_modules, *virtual_path, &vfs);
}
}  // namespace

ProjectLayer::ProjectLayer(
    utils::ThreadWithWorkQueue* thread, RuntimeLayer* runtime_layer,
    const std::optional<std::filesystem::path>& initial_project_path)
    : thread_(thread),
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

  if (current_project_path_ && project_path != *current_project_path_) {
    // If the project path changes, clear out previous results immediately.
    compilation_results_ = std::nullopt;
    runtime_layer_->SetCompiledModule(nullptr);
  }
  // It would be kind of weird to have incoming results arrive that are not
  // what we most recently requested to load.
  pending_compilation_results_ = std::nullopt;
  current_project_path_ = project_path;

  pending_compilation_results_ =
      compilation_thread_
          .Enqueue<utils::ErrorOr<std::shared_ptr<reify::CompiledModule>>>(
              [typescript_modules = domain_visualizer_->GetTypeScriptModules(),
               project_path] {
                return CompileFile(typescript_modules, project_path);
              })
          .watch([this](auto x) {
            thread_->Enqueue([this, x] {
              pending_compilation_results_ = std::nullopt;
              if (std::holds_alternative<utils::CancelledFuture>(x)) {
                compilation_results_ = utils::Error{"Compilation cancelled."};
              } else {
                compilation_results_ = *std::get<1>(x);
              }
              if (auto compiled_module =
                      std::get_if<1>(&(*compilation_results_))) {
                runtime_layer_->SetCompiledModule(*compiled_module);
              }
            });
          });
}

void ProjectLayer::ExecuteImGuiCommands() {
  ImGui::Begin("Project");

  ImGui::PushStyleVar(ImGuiStyleVar_FramePadding, ImVec2(0, 10));
  if (ImGui::BeginMainMenuBar()) {
    {
      DisableIf disable_if_no_project_loaded(!current_project_path_);
      if (ImGui::BeginMenu("Project")) {
        if (ImGui::MenuItem("Recompile", "CTRL+B")) {
          LoadProject(*current_project_path_);
        }
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
    Spinner("compiling spinner", 10.0f, ImVec4{0.2, 0.6, 0.5, 1.0},
            ImVec4{0.1, 0.3, 0.2, 1.0}, 10, 2.5f);
    ImGui::SameLine();
    ImGui::Text(fmt::format("Compiling {}...", current_project_path_->string())
                    .c_str());
  }

  ImGui::End();
}

}  // namespace imgui
}  // namespace typescript_cpp_v8
}  // namespace reify
