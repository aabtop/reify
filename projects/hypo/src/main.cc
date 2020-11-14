#include <chrono>
#include <filesystem>
#include <fstream>
#include <iostream>
#include <optional>
#include <sstream>

#include "CLI/CLI.hpp"
#include "cgal/export_to_file.h"
#include "hypo.h"
#include "reify/typescript_cpp_v8.h"
#include "reify_cpp_v8_interface.h"

using namespace std::chrono;

namespace {
struct CallAndExportResults {
  std::chrono::microseconds call_time;
  std::chrono::microseconds build_time;
  std::chrono::microseconds export_time;
  std::string output_filepath;
};

// Call the TypeScript function named by |function_name| within the given
// |runtime_env|.  Output the results into the file named by
// |output_base_file_path|.  Note that the file should not have an extension,
// one will be chosen automatically depending on the output type.
// If the function's return value is of type hypo::Region2, we will output a
// SVG file that can be opened and viewed by any web browser.  If the function's
// return value is of type hypo::Region3, a STL file will be output which can
// be opened by an STL file viewer.
template <typename T>
std::optional<CallAndExportResults> CallFunctionAndExportOutput(
    reify::RuntimeEnvironment* runtime_env, const std::string& function_name,
    const std::string& output_base_file_path) {
  high_resolution_clock::time_point start_call_time =
      high_resolution_clock::now();

  auto entrypoint_or_error =
      runtime_env->GetExport<reify::Function<T()>>(function_name);
  if (auto error = std::get_if<0>(&entrypoint_or_error)) {
    std::cerr << "Problem finding entrypoint function: " << *error << std::endl;
    return std::nullopt;
  }
  auto entrypoint = &std::get<1>(entrypoint_or_error);

  auto result_or_error = entrypoint->Call();
  if (auto error = std::get_if<0>(&result_or_error)) {
    std::cerr << "Error running function: " << *error << std::endl;
    return std::nullopt;
  }

  auto call_time = std::chrono::duration_cast<std::chrono::microseconds>(
      std::chrono::high_resolution_clock::now() - start_call_time);

  auto results = hypo::cgal::BuildAndExportToFile(std::get<1>(result_or_error),
                                                  output_base_file_path);

  if (!results) {
    return std::nullopt;
  }

  return std::optional<CallAndExportResults>(
      {/*.call_time = */ call_time,
       /*.build_time = */ results->build_time,
       /*.export_time = */ results->export_time,
       /*.output_filepath = */ results->output_filepath});
}

// A non-templated wrapper function around CallFunctionAndExportOutput().
std::optional<CallAndExportResults> BuildOutputAndSaveToFile(
    reify::RuntimeEnvironment* runtime_env,
    const reify::CompiledModule::ExportedSymbol* entry_point_function,
    const std::string& output_base_file_path) {
  if (entry_point_function->HasType<reify::Function<hypo::Region2()>>()) {
    return CallFunctionAndExportOutput<hypo::Region2>(
        runtime_env, entry_point_function->name, output_base_file_path);
  } else if (entry_point_function
                 ->HasType<reify::Function<hypo::Region3()>>()) {
    return CallFunctionAndExportOutput<hypo::Region3>(
        runtime_env, entry_point_function->name, output_base_file_path);
  } else {
    std::cerr << "Exported symbol '" << entry_point_function->name
              << "' has type '" << entry_point_function->typescript_type_string
              << "', which is not a supported type.  Currently only "
                 "parameter-less functions that return Region2 or Region3 "
                 "types are supported."
              << std::endl;
    return std::nullopt;
  }
}

void PrintResultsInformation(std::ostream& out, microseconds compile_time,
                             const CallAndExportResults& results) {
  out << "Successfully produced '" << results.output_filepath << "'. "
      << std::endl;
  out << "  Compile time: " << duration_cast<milliseconds>(compile_time).count()
      << "ms" << std::endl;
  out << "  Call time: "
      << duration_cast<milliseconds>(results.call_time).count() << "ms"
      << std::endl;
  out << "  Build time: "
      << duration_cast<milliseconds>(results.build_time).count() << "ms"
      << std::endl;
  out << "  Export time: "
      << duration_cast<milliseconds>(results.export_time).count() << "ms"
      << std::endl;
}

// See the help documentation text for descriptions of these options.
struct BuildCommandLineParameters {
  std::filesystem::path input_typescript_path;
  std::string function;
  std::filesystem::path output_file_basepath;

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
    int argc, char* argv[]) {
  BuildCommandLineParameters build_clp;
  GenerateProjectDirectoryCommandLineParameters generate_project_directory_clp;

  CLI::App app{"Build geometry declaritively with TypeScript."};

  CLI::App* build =
      app.add_subcommand("build",
                         "Builds geometry from an input TypeScript function. "
                         "Hypo's primary function.");
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
  build
      ->add_option(
          "output_file_basepath,-o,--output_file_basepath",
          build_clp.output_file_basepath,
          "The output file path, without an extension, where the output "
          "will be generated into.  The extension will be chosen based "
          "on the return type of the specified input function.")
      ->required();
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
          "can "
          "be used to setup a Hypo development environment. Note that if this "
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

int Build(const BuildCommandLineParameters& clp) {
  high_resolution_clock::time_point start_time = high_resolution_clock::now();

  // Setup a "chroot" filesystem for module loading based on the directory of
  // the input source file.
  auto absolute_input_source_file =
      std::filesystem::absolute(clp.input_typescript_path);
  auto project_directory =
      clp.project_directory ? std::filesystem::absolute(*clp.project_directory)
                            : absolute_input_source_file.parent_path();

  reify::MountedHostFolderFilesystem virtual_filesystem(project_directory);
  std::optional<std::string> virtual_input_source_path =
      virtual_filesystem.HostPathToVirtualPath(absolute_input_source_file);
  if (!virtual_input_source_path) {
    std::cerr << "Input file " << clp.input_typescript_path
              << " is not contained within the specified project root folder, "
              << project_directory << std::endl;
    return 1;
  }

  // Setup a TypeScript compiler environment.
  reify::CompilerEnvironment compile_env(&virtual_filesystem);

  // Compile the contents of the user's script in memory and check if there were
  // any errors.
  auto module_or_error = compile_env.Compile(
      *virtual_input_source_path, reify::hypo_v8_typescript_declarations());
  if (auto error = std::get_if<0>(&module_or_error)) {
    std::cerr << "Error compiling TypeScript:" << std::endl;
    std::cerr << error->path << ":" << error->line + 1 << ":"
              << error->column + 1 << ": error: " << error->message
              << std::endl;
    return 1;
  }
  auto module = std::get<1>(module_or_error);

  // Setup a V8 runtime environment around the CompiledModule.  This will
  // enable us to call exported functions and query exported values from the
  // module.
  auto runtime_env_or_error = CreateRuntimeEnvironment(module);
  if (auto error = std::get_if<0>(&runtime_env_or_error)) {
    std::cerr << "Error initializing module: " << std::endl;
    std::cerr << *error << std::endl;
    return 1;
  }
  auto runtime_env = &std::get<1>(runtime_env_or_error);

  // Search for a symbol within the compiled module a name specified by the
  // user's command line parameters.
  auto entry_point_function = module->GetExportedSymbol(clp.function);
  if (!entry_point_function) {
    std::cerr << "Error could not find an exported symbol named '"
              << clp.function << "'." << std::endl;
    return 1;
  }

  auto after_compile = high_resolution_clock::now();
  auto compile_time = duration_cast<microseconds>(after_compile - start_time);

  // Check if the user-specified symbol has either a hypo::Region2 or a
  // hypo::Region3 type, these are the only types that we support.  If we do
  // find a supported type, run the function and export the output to a file.
  auto build_results = BuildOutputAndSaveToFile(
      runtime_env, entry_point_function, clp.output_file_basepath.string());

  if (!build_results) {
    return 1;
  }

  PrintResultsInformation(std::cerr, compile_time, *build_results);

  return 0;
}

int GenerateProjectDirectory(
    const GenerateProjectDirectoryCommandLineParameters& clp) {
  bool result = reify::CompilerEnvironment::CreateWorkspaceDirectory(
      clp.project_directory, reify::hypo_v8_typescript_declarations());
  if (result) {
    std::cout << "Created directory " << clp.project_directory
              << ".  You can now add TypeScript files to it." << std::endl;
    return 0;
  } else {
    return 1;
  }
}

}  // namespace

int main(int argc, char* argv[]) {
  auto parse_results = ParseCommandLineParameters(argc, argv);

  if (int* exit_code = std::get_if<int>(&parse_results)) {
    return *exit_code;
  }

  auto clp = std::get<CommandLineParameters>(parse_results);
  if (auto build_clp = std::get_if<BuildCommandLineParameters>(&clp)) {
    return Build(*build_clp);
  } else if (auto generate_project_directory_clp =
                 std::get_if<GenerateProjectDirectoryCommandLineParameters>(
                     &clp)) {
    return GenerateProjectDirectory(*generate_project_directory_clp);
  } else {
    assert(false);
    return 1;
  }
}
