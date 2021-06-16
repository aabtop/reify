#include <fmt/format.h>

#include "reify/typescript_cpp_v8.h"
#include "reify/utils/file_system.h"
#include "reify/utils/future.h"
#include "reify/utils/thread_with_work_queue.h"

namespace reify {

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
                          R"(.*\.ts$)", std::regex_constants::ECMAScript),
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

}  // namespace reify
