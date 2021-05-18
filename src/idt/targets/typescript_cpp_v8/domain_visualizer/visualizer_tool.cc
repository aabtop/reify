#include "reify/typescript_cpp_v8/visualizer_tool.h"

#include <fmt/format.h>

#include <filesystem>

#include "CLI/CLI.hpp"
#include "reify/typescript_cpp_v8.h"
#include "reify/typescript_cpp_v8/domain_visualizer.h"
#include "reify/typescript_cpp_v8/domain_visualizer_gui.h"
#include "reify/window/platform_window_wrapper.h"

namespace reify {
namespace typescript_cpp_v8 {

std::variant<int, VisualizerToolOptions> ParseVisualizerToolOptions(
    const std::string& app_name, const std::string& app_description, int argc,
    char* argv[]) {
  VisualizerToolOptions options;

  CLI::App app{app_description, app_name};

  app.add_option(
         "input_typescript_project_path,-i,--input_typescript_project_path",
         options.project_path,
         "Path to a TypeScript file or project directory.")
      ->check(CLI::ExistingPath);

  try {
    app.parse(argc, argv);
  } catch (const CLI::ParseError& e) {
    return app.exit(e);
  }

  return options;
}

namespace {
utils::ErrorOr<std::shared_ptr<reify::CompiledModule>> CompileVirtualFile(
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
    return utils::Error{fmt::format("{}:{}:{}: error: {}", error->path,
                                    error->line + 1, error->column + 1,
                                    error->message)};
  }

  return std::get<1>(result);
}

utils::ErrorOr<std::shared_ptr<reify::CompiledModule>> CompileFile(
    const std::vector<reify::CompilerEnvironment::InputModule>&
        typescript_input_modules,
    const std::filesystem::path& filepath) {
  if (!std::filesystem::exists(filepath) ||
      std::filesystem::is_directory(filepath)) {
    return utils::Error{fmt::format(
        "Provided path {} either does not exist or is a directory, not a file.",
        filepath.string())};
  }

  // Make or reference a virtual file system based on the current workspace.
  auto absolute_input_source_file = std::filesystem::absolute(filepath);
  auto project_directory = absolute_input_source_file.parent_path();

  reify::MountedHostFolderFilesystem vfs(project_directory);

  auto virtual_path = vfs.HostPathToVirtualPath(absolute_input_source_file);

  if (!virtual_path) {
    return utils::Error{fmt::format(
        "Input file {} is not contained within the project root: {}",
        filepath.string(), vfs.host_root().string())};
  }

  return CompileVirtualFile(typescript_input_modules, *virtual_path, &vfs);
}
}  // namespace

utils::MaybeError RunVisualizerTool(
    const std::string& window_title,
    std::unique_ptr<DomainVisualizer> domain_visualizer,
    const VisualizerToolOptions& options) {
  if (options.project_path) {
    REIFY_UTILS_ASSIGN_OR_RETURN(
        compiled_module, CompileFile(domain_visualizer->GetTypeScriptModules(),
                                     *options.project_path));
    for (auto symbol : compiled_module->exported_symbols()) {
      if (domain_visualizer->CanPreviewSymbol(symbol)) {
        std::mutex mutex;
        std::optional<
            DomainVisualizer::ErrorOr<DomainVisualizer::PreparedSymbol>>
            result;
        std::condition_variable done_cond;
        domain_visualizer->PrepareSymbolForPreview(
            compiled_module, symbol, [&](auto x) {
              std::lock_guard lock(mutex);
              result = x;
              done_cond.notify_all();
            });
        std::unique_lock lock(mutex);
        done_cond.wait(lock, [&] { return result; });

        if (auto error = std::get_if<0>(&(*result))) {
          return utils::Error{error->msg};
        }
        domain_visualizer->Preview(std::get<1>(*result));
      }
    }
  }

  return window::RunPlatformWindowWrapper(
      window_title,
      std::make_unique<reify::typescript_cpp_v8::DomainVisualizerGui>(
          std::move(domain_visualizer)));
}

}  // namespace typescript_cpp_v8
}  // namespace reify
