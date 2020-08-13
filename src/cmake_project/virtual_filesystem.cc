#include <fstream>
#include <numeric>

#include "public_include/reify.h"

namespace REIFY_GENERATED_PROJECT_NAMESPACE {
namespace reify {

MountedHostFolderFilesystem::MountedHostFolderFilesystem(
    const std::filesystem::path& host_root)
    : host_root_(host_root) {
  assert(std::filesystem::is_directory(host_root));
  assert(std::filesystem::exists(host_root));
  assert(host_root.is_absolute());
}

MountedHostFolderFilesystem::FilePath MountedHostFolderFilesystem::GetPath(
    std::string_view virtual_absolute_path) {
  std::filesystem::path host_path = TranslateToHostPath(virtual_absolute_path);

  return FilePath{
      .diagnostics_path = host_path,
      .exists = [host_path]() -> bool {
        return std::filesystem::exists(host_path);
      },
      .get_content = [host_path]() -> std::variant<Error, std::string> {
        if (!std::filesystem::exists(host_path)) {
          return Error{"File '" + host_path.string() + "' could not be found."};
        }

        std::ifstream in(host_path);
        if (in.fail()) {
          return Error{"Error opening file '" + host_path.string() +
                       "' for reading."};
        }
        std::stringstream buffer;
        buffer << in.rdbuf();
        if (in.fail()) {
          return Error{"Error while reading file '" + host_path.string() +
                       "'."};
        }
        return buffer.str();
      },
  };
}

std::optional<std::string> MountedHostFolderFilesystem::HostPathToVirtualPath(
    const std::filesystem::path& host_absolute_path) {
  auto [host_root_end, relative_path_start] = std::mismatch(
      host_root_.begin(), host_root_.end(), host_absolute_path.begin());

  if (host_root_end != host_root_.end()) {
    // This file folder is outside of the project root, so we will refuse to
    // open it.
    return std::nullopt;
  }

  return "/" + std::accumulate(relative_path_start, host_absolute_path.end(),
                               std::filesystem::path{}, std::divides{})
                   .string();
}

std::filesystem::path MountedHostFolderFilesystem::TranslateToHostPath(
    std::string_view virtual_absolute_path) const {
  assert(virtual_absolute_path.length() > 1);
  assert(virtual_absolute_path[0] == '/');
  // Remove the leading '/' and make a path out of the input string.
  std::filesystem::path relative_virtual_path(virtual_absolute_path.substr(1));

  return host_root_ / relative_virtual_path;
}

InMemoryFilesystem::InMemoryFilesystem(const FileMap& file_map)
    : file_map_(file_map) {}

InMemoryFilesystem::FilePath InMemoryFilesystem::GetPath(
    std::string_view virtual_absolute_path) {
  std::string path_as_string(virtual_absolute_path);
  std::string* contents_ptr = nullptr;
  auto found = file_map_.find(path_as_string);
  if (found != file_map_.end()) {
    contents_ptr = &(found->second);
  }

  return FilePath{
      .diagnostics_path = std::string(virtual_absolute_path),
      .exists = [contents_ptr] { return contents_ptr; },
      .get_content = [contents_ptr,
                      path_as_string]() -> std::variant<Error, std::string> {
        if (contents_ptr) {
          return *contents_ptr;
        } else {
          return Error{"File '" + path_as_string + "' not found."};
        }
      }};
}

}  // namespace reify
}  // namespace REIFY_GENERATED_PROJECT_NAMESPACE
