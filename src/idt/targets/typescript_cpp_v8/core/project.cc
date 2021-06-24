#include <fmt/format.h>

#include "reify/typescript_cpp_v8/typescript_cpp_v8.h"
#include "reify/utils/file_system.h"
#include "reify/utils/future.h"
#include "reify/utils/thread_with_work_queue.h"

namespace reify {
namespace typescript_cpp_v8 {

namespace {

utils::ErrorOr<std::unique_ptr<MountedHostFolderFilesystem>>
CreateFilesystemFromDirectoryPath(
    const std::filesystem::path& absolute_project_path) {
  if (!std::filesystem::is_directory(absolute_project_path)) {
    return utils::Error{fmt::format(
        "Input path '{}' was expected to be a directory but it is not.",
        absolute_project_path.string())};
  }

  return std::unique_ptr<MountedHostFolderFilesystem>(
      new MountedHostFolderFilesystem(absolute_project_path));
}

utils::ErrorOr<std::unique_ptr<MountedHostFolderFilesystem>>
CreateFilesystemFromFilePath(
    const std::filesystem::path& absolute_input_source_file) {
  if (std::filesystem::is_directory(absolute_input_source_file)) {
    return utils::Error{fmt::format(
        "Input path '{}' was expected to be a file, not a directory.",
        absolute_input_source_file.string())};
  }

  // Make or reference a virtual file system based on the current workspace.
  auto project_directory = absolute_input_source_file.parent_path();

  return std::unique_ptr<MountedHostFolderFilesystem>(
      new MountedHostFolderFilesystem(project_directory));
}

utils::ErrorOr<std::unique_ptr<MountedHostFolderFilesystem>>
CreateFilesystemFromPath(const std::filesystem::path& path) {
  if (!std::filesystem::exists(path)) {
    return utils::Error{
        fmt::format("Provided path {} does not exist.", path.string())};
  }

  auto absolute_path = std::filesystem::absolute(path);

  if (std::filesystem::is_directory(absolute_path)) {
    return CreateFilesystemFromDirectoryPath(absolute_path);
  } else {
    return CreateFilesystemFromFilePath(absolute_path);
  }
}

std::set<VirtualFilesystem::AbsolutePath> GetAllVisibleTypeScriptSources(
    const MountedHostFolderFilesystem* virtual_filesystem) {
  auto source_file_regex =
      std::regex(R"(.*\.ts$)", std::regex_constants::ECMAScript);
  std::set<VirtualFilesystem::AbsolutePath> source_files;
  for (auto& path : std::filesystem::recursive_directory_iterator(
           virtual_filesystem->host_root())) {
    VirtualFilesystem::AbsolutePath virtual_path =
        *virtual_filesystem->HostPathToVirtualPath(path);
    if (std::regex_search(virtual_path.string(), source_file_regex)) {
      source_files.insert(virtual_path);
    }
  }
  return source_files;
}

std::function<std::set<VirtualFilesystem::AbsolutePath>()>
CreateDefaultBuildFilesGetter(
    const std::filesystem::path& path,
    const MountedHostFolderFilesystem* virtual_filesystem) {
  auto absolute_path = std::filesystem::absolute(path);

  if (std::filesystem::is_directory(absolute_path)) {
    return [virtual_filesystem] {
      return GetAllVisibleTypeScriptSources(virtual_filesystem);
    };
  } else {
    return [virtual_path =
                *virtual_filesystem->HostPathToVirtualPath(absolute_path)] {
      return std::set<VirtualFilesystem::AbsolutePath>{virtual_path};
    };
  }
}
}  // namespace

utils::ErrorOr<HostFilesystemProject> CreateProjectFromPath(
    const std::filesystem::path& path,
    const std::vector<CompilerEnvironment::InputModule>&
        typescript_input_modules) {
  REIFY_UTILS_ASSIGN_OR_RETURN(virtual_filesystem,
                               CreateFilesystemFromPath(path));

  auto virtual_filesystem_raw_ptr = virtual_filesystem.get();

  return HostFilesystemProject{
      std::move(virtual_filesystem),
      std::unique_ptr<CompilerEnvironmentThreadSafe>(
          new CompilerEnvironmentThreadSafe(virtual_filesystem_raw_ptr,
                                            typescript_input_modules)),
  };
}

utils::ErrorOr<HostFilesystemProjectWithBuildFilesGetter>
CreateProjectWithDefaultBuildFilesGetterFromPath(
    const std::filesystem::path& path,
    const std::vector<CompilerEnvironment::InputModule>&
        typescript_input_modules) {
  REIFY_UTILS_ASSIGN_OR_RETURN(
      project, CreateProjectFromPath(path, typescript_input_modules));
  auto build_files_getter =
      CreateDefaultBuildFilesGetter(path, project.virtual_filesystem.get());

  return HostFilesystemProjectWithBuildFilesGetter{
      std::move(project),
      build_files_getter,
  };
}

}  // namespace typescript_cpp_v8
}  // namespace reify
