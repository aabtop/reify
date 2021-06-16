#include <fstream>
#include <iostream>
#include <iterator>
#include <numeric>
#include <sstream>

#include "public_include/reify/typescript_cpp_v8/typescript_cpp_v8.h"

namespace reify {
namespace typescript_cpp_v8 {

namespace {
std::vector<std::string> CollapsePath(const std::vector<std::string>& path) {
  std::vector<std::string> result;
  for (size_t i = 0; i < path.size(); ++i) {
    if (path[i] == ".") {
    } else if (path[i] == ".." && !result.empty() && result.back() != "..") {
      result.pop_back();
    } else {
      result.push_back(path[i]);
    }
  }
  return result;
}

std::vector<std::string> StringToComponents(const std::string& path) {
  std::vector<std::string> components;
  std::string current_component;
  size_t start = 0;
  size_t end = 0;
  for (size_t i = 0; i < path.size(); ++i) {
    if (path[i] == '/') {
      if (end > start) {
        components.push_back(path.substr(start, end));
      }
      start = ++end;
    } else {
      ++end;
    }
  }
  if (end > start) {
    components.push_back(path.substr(start, end));
  }
  return components;
}
std::string ComponentsToString(const std::vector<std::string>& components) {
  std::ostringstream oss;
  for (size_t i = 0; i < components.size(); ++i) {
    if (i != 0) {
      oss << "/";
    }
    oss << components[i];
  }
  return oss.str();
}

std::vector<std::string> ConcatenatePathComponents(
    const std::vector<std::string>& x, const std::vector<std::string>& y) {
  std::vector<std::string> components;
  components.reserve(x.size() + y.size());
  components.insert(components.end(), x.begin(), x.end());
  components.insert(components.end(), y.begin(), y.end());
  return CollapsePath(components);
}
}  // namespace

std::optional<VirtualFilesystem::RelativePath>
VirtualFilesystem::RelativePath::FromString(const std::string& path) {
  if (!path.empty() && path[0] == '/') {
    // This is an absolute path!
    return std::nullopt;
  }
  std::vector<std::string> components = CollapsePath(StringToComponents(path));
  return VirtualFilesystem::RelativePath(components);
}

std::string VirtualFilesystem::RelativePath::string() const {
  return ComponentsToString(components_);
}

std::optional<VirtualFilesystem::AbsolutePath>
VirtualFilesystem::AbsolutePath::FromString(const std::string& path) {
  if (path.empty() || path[0] != '/') {
    // This is not an absolute path!
    return std::nullopt;
  }
  return VirtualFilesystem::AbsolutePath::FromComponents(
      CollapsePath(StringToComponents(path)));
}

std::optional<VirtualFilesystem::AbsolutePath>
VirtualFilesystem::AbsolutePath::FromComponents(
    const std::vector<std::string>& components) {
  if (!components.empty() && components[0] == "..") {
    return std::nullopt;
  }
  return VirtualFilesystem::AbsolutePath(components);
}

std::string VirtualFilesystem::AbsolutePath::string() const {
  return "/" + ComponentsToString(components_);
}

VirtualFilesystem::RelativePath operator/(
    const VirtualFilesystem::RelativePath& x,
    const VirtualFilesystem::RelativePath& y) {
  return VirtualFilesystem::RelativePath(
      ConcatenatePathComponents(x.components(), y.components()));
}

std::optional<VirtualFilesystem::AbsolutePath> operator/(
    const VirtualFilesystem::AbsolutePath& x,
    const VirtualFilesystem::RelativePath& y) {
  return VirtualFilesystem::AbsolutePath::FromComponents(
      ConcatenatePathComponents(x.components(), y.components()));
}

bool operator<(const VirtualFilesystem::AbsolutePath& x,
               const VirtualFilesystem::AbsolutePath& y) {
  return x.components() < y.components();
}

MountedHostFolderFilesystem::MountedHostFolderFilesystem(
    const std::filesystem::path& host_root)
    : host_root_(std::filesystem::canonical(host_root)) {
  assert(std::filesystem::is_directory(host_root));
  assert(std::filesystem::exists(host_root));
  assert(host_root.is_absolute());
}

MountedHostFolderFilesystem::File MountedHostFolderFilesystem::GetFile(
    const AbsolutePath& path) const {
  std::filesystem::path host_path = TranslateToHostPath(path);

  return File(
      host_path.string(),
      [host_path]() -> bool { return std::filesystem::exists(host_path); },
      [host_path]() -> std::variant<Error, std::string> {
        if (!std::filesystem::exists(host_path)) {
          return Error{"File '" + host_path.string() + "' could not be found."};
        }

        std::ifstream in(host_path);
        if (in.fail()) {
          return Error{"Error opening file '" + host_path.string() +
                       "' for reading."};
        }
        std::ostringstream buffer;
        buffer << in.rdbuf();
        if (in.fail()) {
          return Error{"Error while reading file '" + host_path.string() +
                       "'."};
        }
        return buffer.str();
      });
}

std::optional<VirtualFilesystem::AbsolutePath>
MountedHostFolderFilesystem::HostPathToVirtualPath(
    const std::filesystem::path& host_absolute_path) const {
  auto [host_root_end, relative_path_start] = std::mismatch(
      host_root_.begin(), host_root_.end(), host_absolute_path.begin());

  if (host_root_end != host_root_.end()) {
    // This file folder is outside of the project root, so we will refuse to
    // open it.
    return std::nullopt;
  }

  return AbsolutePath::FromComponents(
      std::vector<std::string>(relative_path_start, host_absolute_path.end()));
}

std::filesystem::path MountedHostFolderFilesystem::TranslateToHostPath(
    const AbsolutePath& path) const {
  // Remove the leading '/' and make a path out of the input string.
  std::filesystem::path relative_virtual_path(path.string().substr(1));

  return host_root_ / relative_virtual_path;
}

InMemoryFilesystem::InMemoryFilesystem(const FileMap& file_map)
    : file_map_(file_map) {}

InMemoryFilesystem::File InMemoryFilesystem::GetFile(
    const AbsolutePath& path) const {
  const std::string* contents_ptr = nullptr;
  auto found = file_map_.find(path);
  if (found != file_map_.end()) {
    contents_ptr = &(found->second);
  }

  std::string path_as_string = path.string();
  return File(
      path_as_string, [contents_ptr] { return contents_ptr; },
      [contents_ptr, path_as_string]() -> std::variant<Error, std::string> {
        if (contents_ptr) {
          return *contents_ptr;
        } else {
          return Error{"File '" + path_as_string + "' not found."};
        }
      });
}

}  // namespace typescript_cpp_v8
}  // namespace reify
