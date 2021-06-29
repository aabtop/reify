#ifndef _REIFY_TYPESCRIPT_CPP_V8_VIRTUAL_FILESYSTEM_H_
#define _REIFY_TYPESCRIPT_CPP_V8_VIRTUAL_FILESYSTEM_H_

#include <filesystem>
#include <functional>
#include <map>
#include <optional>
#include <string>
#include <string_view>
#include <variant>

#include "reify/utils/error.h"

namespace reify {
namespace typescript_cpp_v8 {

// Allows specification of how to find and load imported TypeScript modules.
class VirtualFilesystem {
 public:
  class RelativePath {
   public:
    RelativePath(const std::vector<std::string>& components)
        : components_(components) {}
    static std::optional<RelativePath> FromString(const std::string& path);

    std::string string() const;
    const std::vector<std::string>& components() const { return components_; }

   private:
    std::vector<std::string> components_;
  };
  class AbsolutePath {
   public:
    AbsolutePath(AbsolutePath&& x) { *this = std::move(x); }
    const AbsolutePath& operator=(AbsolutePath&& x) {
      components_ = std::move(x.components_);
      x.components_ = std::vector<std::string>();
      return *this;
    }
    AbsolutePath(const AbsolutePath& x) : components_(x.components_) {}
    const AbsolutePath& operator=(const AbsolutePath& x) {
      components_ = x.components_;
      return *this;
    }

    bool operator==(const AbsolutePath& x) const {
      return components_ == x.components_;
    }

    static std::optional<AbsolutePath> FromString(const std::string& path);
    static std::optional<AbsolutePath> FromComponents(
        const std::vector<std::string>& components);

    std::string string() const;
    const std::vector<std::string>& components() const { return components_; }

   private:
    AbsolutePath(const std::vector<std::string>& components)
        : components_(components) {}

    std::vector<std::string> components_;
  };

  struct Error {
    std::string message;
  };
  struct File {
    File(const std::string& diagnostics_path,
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
  virtual File GetFile(const AbsolutePath& path) const = 0;
};

VirtualFilesystem::RelativePath operator/(
    const VirtualFilesystem::RelativePath& x,
    const VirtualFilesystem::RelativePath& y);
std::optional<VirtualFilesystem::AbsolutePath> operator/(
    const VirtualFilesystem::AbsolutePath& x,
    const VirtualFilesystem::RelativePath& y);
bool operator<(const VirtualFilesystem::AbsolutePath& x,
               const VirtualFilesystem::AbsolutePath& y);

// Similar to `chroot`, this VirtualFilesystem implementation will create a
// virtual filesystem with the root set as a folder in the host filesystem.
class MountedHostFolderFilesystem : public VirtualFilesystem {
 public:
  MountedHostFolderFilesystem(const std::filesystem::path& host_root);
  File GetFile(const AbsolutePath& path) const override;

  // Converts a host path into a virtual path.  If the host path is not
  // contained within the mounted folder, a std::nullopt is returend.
  std::optional<AbsolutePath> HostPathToVirtualPath(
      const std::filesystem::path& host_absolute_path) const;

  const std::filesystem::path host_root() const { return host_root_; }

 private:
  std::filesystem::path TranslateToHostPath(const AbsolutePath& path) const;
  const std::filesystem::path host_root_;
};

// Entire filesystem is specified up-front via a file to content mapping.
class InMemoryFilesystem : public VirtualFilesystem {
 public:
  using FileMap = std::map<AbsolutePath, std::string>;
  InMemoryFilesystem(const FileMap& file_map);
  File GetFile(const AbsolutePath& path) const override;

 private:
  const FileMap file_map_;
};

}  // namespace typescript_cpp_v8
}  // namespace reify

#endif  // _REIFY_TYPESCRIPT_CPP_V8_VIRTUAL_FILESYSTEM_H_
