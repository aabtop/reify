#include "reify/utils/file_system.h"

#include <gtest/gtest.h>

#include <fstream>

namespace {
void TouchFile(const std::filesystem::path& path) { std::ofstream o(path); }
}  // namespace

TEST(FileSystemTest, TemporaryDirectoryTest) {
  std::optional<std::filesystem::path> temp_directory_path;
  {
    reify::utils::TemporaryDirectory temp_directory;
    temp_directory_path = temp_directory.path();
    EXPECT_TRUE(std::filesystem::exists(*temp_directory_path));

    bool create_directories_result =
        std::filesystem::create_directories(*temp_directory_path / "a/b/c");
    EXPECT_TRUE(create_directories_result);
  }

  EXPECT_FALSE(std::filesystem::exists(*temp_directory_path));
}

TEST(FileSystemTest, MakeRelativePathTest) {
  EXPECT_EQ("bar", reify::utils::MakeRelativePath("/foo", "/foo/bar"));
  EXPECT_EQ("b/c", reify::utils::MakeRelativePath("/foo/a", "/foo/a/b/c"));
}

// Demonstrate some basic assertions.
TEST(FileSystemTest, FindMatchingFilenamesRecursivelyTest) {
  reify::utils::TemporaryDirectory temp_directory;

  TouchFile(temp_directory.path() / "foo.txt");
  TouchFile(temp_directory.path() / "footxt");
  TouchFile(temp_directory.path() / "bar.jazz");
  TouchFile(temp_directory.path() / "foobar.txt");

  std::filesystem::path subdirectory = temp_directory.path() / "a/b/c";
  std::filesystem::create_directories(subdirectory);
  TouchFile(subdirectory / "1.txt");
  TouchFile(subdirectory / "2.txt");
  TouchFile(subdirectory / "3.jazz");

  std::vector<std::filesystem::path> results =
      reify::utils::FindMatchingFilenamesRecursively(temp_directory.path(),
                                                     std::regex("\\.txt$"));

  const std::vector<std::filesystem::path> EXPECTED_RESULTS = {
      "a/b/c/1.txt", "a/b/c/2.txt", "foo.txt", "foobar.txt"};

  // Process the results by sorting and getting rid of the temp dir prefix.
  for (auto& path : results) {
    path = *reify::utils::MakeRelativePath(temp_directory.path(), path);
  }
  std::sort(results.begin(), results.end());

  EXPECT_EQ(EXPECTED_RESULTS, results);
}
