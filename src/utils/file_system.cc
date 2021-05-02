#include "reify/utils/file_system.h"

#include <cassert>
#include <random>

namespace reify {
namespace utils {

namespace {
std::string CreateRandomLetterString() {
  std::ostringstream oss;
  const int kNumRandomCharacters = 10;
  std::random_device rd;
  std::mt19937 gen(rd());
  std::uniform_int_distribution<> random_character('a', 'z');

  for (int i = 0; i < kNumRandomCharacters; ++i) {
    oss << static_cast<char>(random_character(gen));
  }
  return oss.str();
}
}  // namespace

TemporaryDirectory::TemporaryDirectory() {
  constexpr int MAX_TRY_COUNT = 10;
  for (int i = 0; i < MAX_TRY_COUNT; ++i) {
    path_ = std::filesystem::temp_directory_path() / CreateRandomLetterString();
    if (std::filesystem::create_directories(path_)) {
      // We successfully created a new temporary directory, we're done.
      return;
    }
  }

  // We're not going to have fun if we weren't able to create a directory
  // before now.
  assert(false);
}

TemporaryDirectory::~TemporaryDirectory() {
  std::filesystem::remove_all(path_);
}

std::optional<std::filesystem::path> MakeRelativePath(
    const std::filesystem::path& absolute_root_path,
    const std::filesystem::path& absolute_path) {
  auto [root_path_end, relative_path_start] =
      std::mismatch(absolute_root_path.begin(), absolute_root_path.end(),
                    absolute_path.begin());

  if (root_path_end != absolute_root_path.end()) {
    // This file folder is outside of the project root, so we will refuse to
    // open it.
    return std::nullopt;
  }

  return std::accumulate(relative_path_start, absolute_path.end(),
                         std::filesystem::path{}, std::divides{});
}

std::vector<std::filesystem::path> FindMatchingFilenamesRecursively(
    const std::filesystem::path& root, const std::regex& pattern) {
  std::vector<std::filesystem::path> results;
  if (!std::filesystem::is_directory(root)) {
    return results;
  }

  for (auto& path : std::filesystem::recursive_directory_iterator(root)) {
    if (std::regex_search(path.path().string(), pattern)) {
      results.push_back(path);
    }
  }
  return results;
}

}  // namespace utils
}  // namespace reify
