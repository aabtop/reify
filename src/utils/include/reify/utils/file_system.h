#ifndef _SRC_UTILS_INCLUDE_REIFY_UTILS_FILE_SYSTEM_H_
#define _SRC_UTILS_INCLUDE_REIFY_UTILS_FILE_SYSTEM_H_

#include <filesystem>
#include <optional>
#include <regex>

namespace reify {
namespace utils {

// Uses RAII to temporarily create a directory and clean up on destruction.
// Useful for unit tests.
class TemporaryDirectory {
 public:
  TemporaryDirectory();
  ~TemporaryDirectory();

  const std::filesystem::path& path() const { return path_; }

 private:
  std::filesystem::path path_;
};

// Makes an absolute path relative to a specified root path by removing the root
// path's prefix from the absolute path. Returns `std::nullopt` if the
// absolute path of the file does not have `root_path` as a prefix.
std::optional<std::filesystem::path> MakeRelativePath(
    const std::filesystem::path& root_path,
    const std::filesystem::path& absolute_path);

std::vector<std::filesystem::path> FindMatchingFilenamesRecursively(
    const std::filesystem::path& root, const std::regex& pattern);

}  // namespace utils
}  // namespace reify

#endif  // _SRC_UTILS_INCLUDE_REIFY_UTILS_FILE_SYSTEM_H_