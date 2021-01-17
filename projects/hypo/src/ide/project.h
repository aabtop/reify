#ifndef _IDE_PROJECT_H_
#define _IDE_PROJECT_H_

#include <filesystem>
#include <optional>
#include <variant>

#include "reify/typescript_cpp_v8.h"

class Project {
 public:
  Project(const std::filesystem::path& filepath,
          std::vector<reify::CompilerEnvironment::InputModule>&&
              typescript_input_modules);

  using CompileError = std::string;
  std::variant<std::shared_ptr<reify::CompiledModule>, CompileError>
  CompileFile(const std::filesystem::path& filepath);

 private:
  std::filesystem::path current_filepath_;
  const std::vector<reify::CompilerEnvironment::InputModule>
      initial_input_modules_;
  std::optional<reify::MountedHostFolderFilesystem> vfs_;
  std::optional<reify::CompilerEnvironment> compile_env_;
};

#endif  // _IDE_PROJECT_H_
