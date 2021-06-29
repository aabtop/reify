#include "src/idt/targets/typescript_cpp_v8/ide/compilation.h"

#include <filesystem>
#include <sstream>

namespace reify {
namespace typescript_cpp_v8 {
namespace ide {

namespace {

CompilerEnvironment::CompileResults CompileVirtualFile(
    const std::vector<CompilerEnvironment::InputModule>&
        typescript_input_modules,
    const VirtualFilesystem::AbsolutePath& path, VirtualFilesystem* vfs) {
  // V8 doesn't like being initialized on one thread and then used on another,
  // so we just recreate the compiler environment each time.  This means we
  // need to reload the TypeScript compiler every time we want to compile
  // something, which kind of sucks.  The output of the Compile() call is
  // completely independent of the compiler environment, so at least we're
  // safe there.

  CompilerEnvironment compile_env(vfs, &typescript_input_modules);
  return compile_env.Compile(path);
}

}  // namespace

FileProject CreateFileProject(
    const std::filesystem::path& path,
    const std::vector<CompilerEnvironment::InputModule>&
        typescript_input_modules) {
  return FileProject{
      path,
      std::get<1>(CreateProjectWithDefaultBuildFilesGetterFromPath(
          path, typescript_input_modules)),
  };
}

CompilerEnvironment::CompileResults CompileContents(
    const std::vector<
        reify::typescript_cpp_v8::CompilerEnvironment::InputModule>&
        typescript_input_modules,
    const std::string& contents) {
  auto input_path =
      *VirtualFilesystem::AbsolutePath::FromComponents({"untitled.ts"});
  reify::typescript_cpp_v8::InMemoryFilesystem in_memory_filesystem(
      reify::typescript_cpp_v8::InMemoryFilesystem::FileMap{
          {input_path, contents}});

  return CompileVirtualFile(typescript_input_modules, input_path,
                            &in_memory_filesystem);
}

std::string CompileErrorToString(const CompileError& error) {
  std::ostringstream oss;
  oss << error.path << ":" << error.line + 1 << ":" << error.column + 1
      << ": error: " << error.message;
  return oss.str();
}

}  // namespace ide
}  // namespace typescript_cpp_v8
}  // namespace reify