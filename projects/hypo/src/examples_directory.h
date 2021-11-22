#ifndef _HYPO_EXAMPLES_DIRECTORY_H_
#define _HYPO_EXAMPLES_DIRECTORY_H_

#include <filesystem>
#include <optional>

namespace hypo {
std::optional<std::filesystem::path> GetExamplesDirectory(int argc,
                                                          char* argv[]);
}

#endif  // _HYPO_EXAMPLES_DIRECTORY_H_
