#include "examples_directory.h"

#include <iostream>

namespace hypo {

std::optional<std::filesystem::path> GetExamplesDirectory(int argc,
                                                          char* argv[]) {
  if (argc == 0) {
    std::cerr << "argc == 0" << std::endl;
    return std::nullopt;
  }

  if (!std::filesystem::exists(argv[0])) {
    std::cerr << "Could not find executable path (argv[0]): " << argv[0]
              << std::endl;
    return std::nullopt;
  }

  std::filesystem::path current_executable_path =
      std::filesystem::canonical(argv[0]);

  const auto relative_path =
      std::filesystem::path("src/playground_workspace/example_scripts");
  auto example_project_path_from_exe_dir =
      current_executable_path.parent_path() / relative_path;
  if (!std::filesystem::exists(example_project_path_from_exe_dir)) {
    std::cerr << "Could not find examples directory at: "
              << example_project_path_from_exe_dir << std::endl;
    return std::nullopt;
  }

  return example_project_path_from_exe_dir;
}

}  // namespace hypo
