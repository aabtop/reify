#include "reify/typescript_cpp_v8/command_line_tool.h"

#include <chrono>
#include <iostream>
#include <optional>

using namespace std::chrono;

namespace reify {
namespace typescript_cpp_v8 {

// See the help documentation text for descriptions of these options.
struct BuildCommandLineParameters {
  std::filesystem::path input_typescript_path;
  std::string function;

  std::optional<std::filesystem::path> project_directory;
};
struct GenerateProjectDirectoryCommandLineParameters {
  std::filesystem::path project_directory;
};
using CommandLineParameters =
    std::variant<BuildCommandLineParameters,
                 GenerateProjectDirectoryCommandLineParameters>;

// Returns either a system exit error code, or parsed and validated command
// line parameters.
std::variant<int, CommandLineParameters> ParseCommandLineParameters(
    const std::string& app_name, const std::string& app_description,
    const std::function<void(CLI::App*)>& add_options_hook, int argc,
    char* argv[]) {
  BuildCommandLineParameters build_clp;
  GenerateProjectDirectoryCommandLineParameters generate_project_directory_clp;

  CLI::App app{app_description, app_name};

  CLI::App* build = app.add_subcommand("build", app_description);
  CLI::App* generate_project_directory =
      app.add_subcommand("generate_project_directory",
                         "Used to initialize projects with TypeScript "
                         "definition files to enable full IDE support.");

  // We require exactly one subcommand to be given.
  app.require_subcommand(1);

  build
      ->add_option("input_typescript_path,-i,--input_typescript_path",
                   build_clp.input_typescript_path,
                   "Path to a TypeScript file which defines the function "
                   "describing the geometry to generate.")
      ->required()
      ->check(CLI::ExistingFile);
  build
      ->add_option("function,-f,--function", build_clp.function,
                   "The function within the `input_typescript_path` file which "
                   "will be executed to create the geomtry.")
      ->required();
  if (add_options_hook) {
    add_options_hook(build);
  }
  build
      ->add_option(
          "-r,--project_directory", build_clp.project_directory,
          "Specifies a directory as the project's root directory, "
          "deciding the root folder of the virtual file system in which "
          "the function is executed.  This for example affects things "
          "like the TypeScript relative imports root folder. If it is "
          "not specified, it will be assigned the directory containing "
          "`input_typescript_path`.")
      ->check(CLI::ExistingDirectory);

  generate_project_directory
      ->add_option(
          "project_directory,-w,--project_directory",
          generate_project_directory_clp.project_directory,
          "Instead of generating geometry, a workspace directory will be "
          "created in the specified directory where TypeScript declaration "
          "files and will be created such that auto-complete and other IDE "
          "tools will be enabled while working within that directory.  This "
          "can be used to setup a development environment. Note that if this "
          "flag is provided, no geometry will be generated.")
      ->required()
      ->check(CLI::NonexistentPath);

  CLI11_PARSE(app, argc, argv);

  if (build->count()) {
    return build_clp;
  } else if (generate_project_directory->count()) {
    return generate_project_directory_clp;
  } else {
    assert(false);
    return build_clp;
  }
}

int GenerateProjectDirectory(
    const GenerateProjectDirectoryCommandLineParameters& clp,
    const std::vector<CompilerEnvironment::InputModule>&
        typescript_input_modules) {
  bool result = reify::CompilerEnvironment::CreateWorkspaceDirectory(
      clp.project_directory, typescript_input_modules);
  if (result) {
    std::cout << "Created directory " << clp.project_directory
              << ".  You can now add TypeScript files to it." << std::endl;
    return 0;
  } else {
    return 1;
  }
}

std::variant<int, std::unique_ptr<CommandLineToolParseResult>> Build(
    const BuildCommandLineParameters& clp,
    const std::vector<CompilerEnvironment::InputModule>&
        typescript_input_modules) {
  auto result = std::make_unique<CommandLineToolParseResult>();

  high_resolution_clock::time_point start_time = high_resolution_clock::now();

  // Setup a "chroot" filesystem for module loading based on the directory of
  // the input source file.
  auto absolute_input_source_file =
      std::filesystem::absolute(clp.input_typescript_path);
  auto project_directory =
      clp.project_directory ? std::filesystem::absolute(*clp.project_directory)
                            : absolute_input_source_file.parent_path();

  result->virtual_filesystem.emplace(project_directory);
  std::optional<std::string> virtual_input_source_path =
      result->virtual_filesystem->HostPathToVirtualPath(
          absolute_input_source_file);
  if (!virtual_input_source_path) {
    std::cerr << "Input file " << clp.input_typescript_path
              << " is not contained within the specified project root folder, "
              << project_directory << std::endl;
    return 1;
  }

  // Setup a TypeScript compiler environment.
  result->compile_env.emplace(&(*result->virtual_filesystem),
                              &typescript_input_modules);

  // Compile the contents of the user's script in memory and check if there were
  // any errors.
  auto compiled_module_or_error =
      result->compile_env->Compile(*virtual_input_source_path);
  if (auto error = std::get_if<0>(&compiled_module_or_error)) {
    std::cerr << "Error compiling TypeScript:" << std::endl;
    std::cerr << error->path << ":" << error->line + 1 << ":"
              << error->column + 1 << ": error: " << error->message
              << std::endl;
    return 1;
  }
  result->compiled_module = std::get<1>(compiled_module_or_error);

  // Setup a V8 runtime environment around the CompiledModule.  This will
  // enable us to call exported functions and query exported values from the
  // module.
  auto runtime_env_or_error = CreateRuntimeEnvironment(result->compiled_module);
  if (auto error = std::get_if<0>(&runtime_env_or_error)) {
    std::cerr << "Error initializing module: " << std::endl;
    std::cerr << *error << std::endl;
    return 1;
  }
  result->runtime_env.emplace(std::move(std::get<1>(runtime_env_or_error)));

  // Search for a symbol within the compiled module a name specified by the
  // user's command line parameters.
  auto entry_point_symbol =
      result->compiled_module->GetExportedSymbol(clp.function);
  if (!entry_point_symbol) {
    std::cerr << "Error could not find an exported symbol named '"
              << clp.function << "'." << std::endl;
    return 1;
  }
  result->entry_point_symbol = entry_point_symbol;

  auto after_compile = high_resolution_clock::now();
  result->compile_time =
      duration_cast<microseconds>(after_compile - start_time);

  return std::move(result);
}

std::variant<int, std::unique_ptr<CommandLineToolParseResult>>
CommandLineToolParse(const CommandLineToolParameters& params, int argc,
                     char* argv[]) {
  auto parse_results =
      ParseCommandLineParameters(params.app_name, params.app_description,
                                 params.add_options_hook, argc, argv);

  if (int* exit_code = std::get_if<int>(&parse_results)) {
    return *exit_code;
  }

  auto clp = std::get<CommandLineParameters>(parse_results);
  if (auto build_clp = std::get_if<BuildCommandLineParameters>(&clp)) {
    return Build(*build_clp, params.typescript_input_modules);
  } else if (auto generate_project_directory_clp =
                 std::get_if<GenerateProjectDirectoryCommandLineParameters>(
                     &clp)) {
    return GenerateProjectDirectory(*generate_project_directory_clp,
                                    params.typescript_input_modules);
  } else {
    assert(false);
    return 1;
  }
}

}  // namespace typescript_cpp_v8
}  // namespace reify
