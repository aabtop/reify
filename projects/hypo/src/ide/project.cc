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
}

std::variant<std::shared_ptr<reify::CompiledModule>, Project::CompileError>
Project::CompileFile(const std::filesystem::path& filepath) {
  auto virtual_path = FilepathToVirtualFilepath(filepath);

  if (!virtual_path) {
    return "Input file " + filepath.string() +
           " is not contained within the project root: " +
           vfs_->host_root().string();
  }

  // V8 doesn't like being initialized on one thread and then used on another,
  // so we just recreate the compiler environment each time.  This means we need
  // to reload the TypeScript compiler every time we want to compile something,
  // which kind of sucks.  The output of the Compile() call is completely
  // independent of the compiler environment, so at least we're safe there.
  auto result = [&]() {
    reify::CompilerEnvironment compile_env(&(*vfs_), &initial_input_modules_);
    return compile_env.Compile(*virtual_path);
  }();

  if (auto error = std::get_if<0>(&result)) {
    std::ostringstream oss;
    oss << error->path << ":" << error->line + 1 << ":" << error->column + 1
        << ": error: " << error->message;
    return oss.str();
  }

  compiled_modules_.clear();
  compiled_modules_[*virtual_path] = std::get<1>(result);

  return std::get<1>(result);
}

std::optional<std::shared_ptr<reify::CompiledModule>>
Project::GetCompiledModules(const std::filesystem::path& filepath) const {
  auto virtual_path = FilepathToVirtualFilepath(filepath);
  if (!virtual_path) {
    return std::nullopt;
  }

  auto found = compiled_modules_.find(*virtual_path);
  if (found == compiled_modules_.end()) {
    return std::nullopt;
  }

  return found->second;
}

std::optional<std::string> Project::FilepathToVirtualFilepath(
    const std::filesystem::path& filepath) const {
  auto absolute_filepath = std::filesystem::absolute(filepath);

  return vfs_->HostPathToVirtualPath(absolute_filepath);
}
