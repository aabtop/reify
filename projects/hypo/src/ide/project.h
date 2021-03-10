#ifndef _IDE_PROJECT_H_
#define _IDE_PROJECT_H_

#include <filesystem>
#include <map>
#include <optional>
#include <variant>

#include "reify/typescript_cpp_v8.h"
#include "src/ide/thread_checker.h"

class Project {
 public:
  Project(const std::filesystem::path& filepath,
          std::vector<reify::CompilerEnvironment::InputModule>&&
              typescript_input_modules);

  using CompileError = std::string;
  using CompileResult =
      std::variant<std::shared_ptr<reify::CompiledModule>, CompileError>;
  CompileResult CompileFile(const std::filesystem::path& filepath);

  std::optional<std::shared_ptr<reify::CompiledModule>> GetCompiledModules(
      const std::filesystem::path& filepath) const;

 private:
  std::optional<std::string> FilepathToVirtualFilepath(
      const std::filesystem::path& filepath) const;

  std::filesystem::path current_filepath_;
  const std::vector<reify::CompilerEnvironment::InputModule>
      initial_input_modules_;
  std::optional<reify::MountedHostFolderFilesystem> vfs_;

  // Map from virtual filepath to compiled module.
  std::map<std::string, std::shared_ptr<reify::CompiledModule>>
      compiled_modules_;
};

#endif  // _IDE_PROJECT_H_
