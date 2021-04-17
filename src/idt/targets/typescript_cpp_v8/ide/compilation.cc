#include "src/idt/targets/typescript_cpp_v8/ide/compilation.h"

#include <filesystem>
#include <sstream>

namespace reify {
namespace typescript_cpp_v8 {
namespace ide {

namespace {

CompileResult CompileVirtualFile(
    const std::vector<reify::CompilerEnvironment::InputModule>&
        typescript_input_modules,
    const std::string& virtual_filepath, reify::VirtualFilesystem* vfs) {
  // V8 doesn't like being initialized on one thread and then used on another,
  // so we just recreate the compiler environment each time.  This means we
  // need to reload the TypeScript compiler every time we want to compile
  // something, which kind of sucks.  The output of the Compile() call is
  // completely independent of the compiler environment, so at least we're
  // safe there.
  auto result = [&]() {
    reify::CompilerEnvironment compile_env(vfs, &typescript_input_modules);
    return compile_env.Compile(virtual_filepath);
  }();

  if (auto error = std::get_if<0>(&result)) {
    std::ostringstream oss;
    oss << error->path << ":" << error->line + 1 << ":" << error->column + 1
        << ": error: " << error->message;
    return oss.str();
  }

  return std::get<1>(result);
}

}  // namespace

CompileResult CompileFile(
    const std::vector<reify::CompilerEnvironment::InputModule>&
        typescript_input_modules,
    const std::filesystem::path& filepath) {
  // Make or reference a virtual file system based on the current workspace.
  auto absolute_input_source_file = std::filesystem::absolute(filepath);
  auto project_directory = absolute_input_source_file.parent_path();

  reify::MountedHostFolderFilesystem vfs(project_directory);

  auto virtual_path = vfs.HostPathToVirtualPath(absolute_input_source_file);

  if (!virtual_path) {
    return "Input file " + filepath.string() +
           " is not contained within the project root: " +
           vfs.host_root().string();
  }

  auto maybe_compiled_module =
      CompileVirtualFile(typescript_input_modules, *virtual_path, &vfs);
  if (auto* error = std::get_if<0>(&maybe_compiled_module)) {
    return *error;
  }

  return maybe_compiled_module;
}

CompileResult CompileContents(
    const std::vector<reify::CompilerEnvironment::InputModule>&
        typescript_input_modules,
    const std::string& contents) {
  const std::string input_virtual_filepath = "/untitled.ts";
  reify::InMemoryFilesystem in_memory_filesystem(
      reify::InMemoryFilesystem::FileMap{{input_virtual_filepath, contents}});

  return CompileVirtualFile(typescript_input_modules, input_virtual_filepath,
                            &in_memory_filesystem);
}

}  // namespace ide
}  // namespace typescript_cpp_v8
}  // namespace reify