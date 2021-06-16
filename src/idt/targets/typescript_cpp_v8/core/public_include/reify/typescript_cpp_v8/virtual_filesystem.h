#ifndef _REIFY_TYPESCRIPT_CPP_V8_VIRTUAL_FILESYSTEM_H_
#define _REIFY_TYPESCRIPT_CPP_V8_VIRTUAL_FILESYSTEM_H_

#include <filesystem>
#include <functional>
#include <string>
#include <string_view>
#include <unordered_map>
#include <variant>

#include "reify/utils/error.h"

namespace reify {
namespace typescript_cpp_v8 {

// Allows specification of how to find and load imported TypeScript modules.
class VirtualFilesystem {
 public:
  struct Error {
    std::string message;
  };
  struct FilePath {
    FilePath(
        const std::string& diagnostics_path,
        const std::function<bool()>& exists,
        const std::function<std::variant<Error, std::string>()>& get_content)
        : diagnostics_path(diagnostics_path),
          exists(exists),
          get_content(get_content) {}

    // The path that will represent this file for the sake of diagnostics,
    // for example this is the path that will be displayed to the user when
    // an error occurs.  By letting this differ from the virtual file path,
    // we enable error messages to be meaningful to users while the scripts
    // continue to be sandboxed.
    std::string diagnostics_path;
    // Returns true if the file exists, otherwise returns false.
    std::function<bool()> exists;
    // A function that, when called, will return the contents of the file.
    std::function<std::variant<Error, std::string>()> get_content;
  };

  // Returns a handle to a file, given a virtual file path.  The virtual
  // file system is the one in which the TypeScript compiler will operate.
  // Virtual absolute paths always start with a `/`.
  virtual FilePath GetPath(std::string_view virtual_absolute_path) const = 0;
};

// Similar to `chroot`, this VirtualFilesystem implementation will create a
// virtual filesystem with the root set as a folder in the host filesystem.
class MountedHostFolderFilesystem : public VirtualFilesystem {
 public:
  MountedHostFolderFilesystem(const std::filesystem::path& host_root);
  FilePath GetPath(std::string_view virtual_absolute_path) const override;

  // Converts a host path into a virtual path.  If the host path is not
  // contained within the mounted folder, a std::nullopt is returend.
  std::optional<std::string> HostPathToVirtualPath(
      const std::filesystem::path& host_absolute_path) const;

  const std::filesystem::path host_root() const { return host_root_; }

 private:
  std::filesystem::path TranslateToHostPath(
      std::string_view virtual_absolute_path) const;
  const std::filesystem::path host_root_;
};

// Entire filesystem is specified up-front via a filepath to content mapping.
class InMemoryFilesystem : public VirtualFilesystem {
 public:
  using FileMap = std::unordered_map<std::string, std::string>;
  InMemoryFilesystem(const FileMap& file_map);
  FilePath GetPath(std::string_view virtual_absolute_path) const override;

 private:
  const FileMap file_map_;
};

}  // namespace typescript_cpp_v8
}  // namespace reify

#endif  // _REIFY_TYPESCRIPT_CPP_V8_VIRTUAL_FILESYSTEM_H_
