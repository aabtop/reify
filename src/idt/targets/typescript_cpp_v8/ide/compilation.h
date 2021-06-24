#ifndef _IDE_COMPILATION_H_
#define _IDE_COMPILATION_H_

#include <filesystem>
#include <map>
#include <optional>
#include <variant>

#include "reify/typescript_cpp_v8/typescript_cpp_v8.h"

namespace reify {
namespace typescript_cpp_v8 {
namespace ide {

struct FileProject {
  std::filesystem::path filepath;
  HostFilesystemProjectWithBuildFilesGetter project;
};

FileProject CreateFileProject(
    const std::filesystem::path& path,
    const std::vector<CompilerEnvironment::InputModule>&
        typescript_input_modules);

CompilerEnvironment::CompileResults CompileContents(
    const std::vector<CompilerEnvironment::InputModule>&
        typescript_input_modules,
    const std::string& contents);

std::string CompileErrorToString(const CompileError& error);

}  // namespace ide
}  // namespace typescript_cpp_v8
}  // namespace reify

#endif  // _IDE_COMPILATION_H_
