#include "src/ide/project.h"

#include <filesystem>
#include <sstream>

Project::Project(const std::filesystem::path& filepath,
                 std::vector<reify::CompilerEnvironment::InputModule>&&
                     typescript_input_modules)
    : current_filepath_(filepath),
      initial_input_modules_(std::move(typescript_input_modules)) {
  // Make or reference a virtual file system based on the current workspace.
  auto absolute_input_source_file =
      std::filesystem::absolute(current_filepath_);
  auto project_directory = absolute_input_source_file.parent_path();

  vfs_.emplace(project_directory);

  compile_env_.emplace(&(*vfs_), &initial_input_modules_);
}

std::variant<std::shared_ptr<reify::CompiledModule>, Project::CompileError>
Project::CompileFile(const std::filesystem::path& filepath) {
  thread_checker_.Check();

  auto absolute_filepath = std::filesystem::absolute(filepath);

  std::optional<std::string> virtual_path =
      vfs_->HostPathToVirtualPath(absolute_filepath);

  if (!virtual_path) {
    return "Input file " + absolute_filepath.string() +
           " is not contained within the project root: " +
           vfs_->host_root().string();
  }

  auto result = compile_env_->Compile(*virtual_path);

  if (auto error = std::get_if<0>(&result)) {
    std::ostringstream oss;
    oss << error->path << ":" << error->line + 1 << ":" << error->column + 1
        << ": error: " << error->message;
    return oss.str();
  }

  return std::get<1>(result);
}
