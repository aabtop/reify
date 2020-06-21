#include <chrono>
#include <fstream>
#include <iostream>
#include <optional>
#include <sstream>

#include "cgal/export_to_file.h"
#include "hypo.h"
#include "reify.h"

namespace {
// Load an entire file's contents into memory.
std::optional<std::string> LoadFile(const char* filename) {
  std::ifstream in(filename);
  if (in.fail()) return std::nullopt;

  std::stringstream buffer;
  buffer << in.rdbuf();
  return buffer.str();
}

// Call the TypeScript function named by |function_name| within the given
// |runtime_env|.  Output the results into the file named by
// |output_base_file_path|.  Note that the file should not have an extension,
// one will be chosen automatically depending on the output type.
// If the function's return value is of type hypo::Region2, we will output a
// SVG file that can be opened and viewed by any web browser.  If the function's
// return value is of type hypo::Region3, a STL file will be output which can
// be opened by an STL file viewer.
template <typename T>
int CallFunctionAndExportOutput(hypo::reify::RuntimeEnvironment* runtime_env,
                                const char* function_name,
                                const char* output_base_file_path) {
  auto entrypoint_or_error =
      runtime_env->GetExport<hypo::reify::Function<T()>>(function_name);
  if (auto error = std::get_if<0>(&entrypoint_or_error)) {
    std::cerr << "Problem finding entrypoint function: " << *error << std::endl;
    return 1;
  }
  auto entrypoint = &std::get<1>(entrypoint_or_error);

  auto result_or_error = entrypoint->Call();
  if (auto error = std::get_if<0>(&result_or_error)) {
    std::cerr << "Error running function: " << *error << std::endl;
    return 1;
  }

  bool results = hypo::cgal::ExportToFile(std::get<1>(result_or_error),
                                          output_base_file_path);

  return results ? 0 : 1;
}

int BuildOutputAndSaveToFile(
    hypo::reify::RuntimeEnvironment* runtime_env,
    const hypo::reify::CompiledModule::ExportedSymbol* entry_point_function,
    const char* function_name,  // TODO: This seems redundant given
                                // |entry_point_function|, remove it.
    const char* output_base_file_path) {
  if (entry_point_function->HasType<hypo::reify::Function<hypo::Region2()>>()) {
    return CallFunctionAndExportOutput<hypo::Region2>(
        runtime_env, function_name, output_base_file_path);
  } else if (entry_point_function
                 ->HasType<hypo::reify::Function<hypo::Region3()>>()) {
    return CallFunctionAndExportOutput<hypo::Region3>(
        runtime_env, function_name, output_base_file_path);
  } else {
    std::cerr << "Exported symbol '" << entry_point_function->name
              << "' has type '" << entry_point_function->typescript_type_string
              << "', which is not a supported type.  Currently only "
                 "parameter-less functions that return Region2 or Region3 "
                 "types are supported."
              << std::endl;
    return 1;
  }
}

}  // namespace

int main(int argc, char* argv[]) {
  if (argc < 4) {
    std::cerr << "USAGE: " << argv[0]
              << " INPUT_FILE FUNCTION_NAME OUTPUT_FILE_WITHOUT_EXTENSION"
              << std::endl;
    return 1;
  }

  using namespace std::chrono;

  high_resolution_clock::time_point start_time = high_resolution_clock::now();

  // Setup a TypeScript compiler environment.
  hypo::reify::CompilerEnvironment compile_env;

  // Load the contents of the referenced input file into memory.
  auto input_file = LoadFile(argv[1]);
  if (!input_file) {
    std::cerr << "Error loading file '" << argv[1] << "'." << std::endl;
    return 1;
  }

  // Compile the contents of the user's script in memory and check if there were
  // any errors.
  auto module_or_error = compile_env.Compile(argv[1], *input_file);
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

  const char* function_name = argv[2];
  const char* output_base_file_path = argv[3];

  // Search for a symbol within the compiled module a name specified by the
  // user's command line parameters.
  auto entry_point_function = module->GetExportedSymbol(function_name);
  if (!entry_point_function) {
    std::cerr << "Error could not find an exported symbol named '"
              << function_name << "'." << std::endl;
    return 1;
  }

  auto after_compile = high_resolution_clock::now();
  auto compile_time = after_compile - start_time;

  // Check if the user-specified symbol has either a hypo::Region2 or a
  // hypo::Region3 type, these are the only types that we support.  If we do
  // find a supported type, run the function and export the output to a file.
  int return_code = BuildOutputAndSaveToFile(
      runtime_env, entry_point_function, function_name, output_base_file_path);
  auto after_build_output = high_resolution_clock::now();
  auto build_output_time = after_build_output - after_compile;

  if (return_code == 0) {
    std::cerr << "Build success. (Compile time: "
              << std::chrono::duration_cast<std::chrono::milliseconds>(
                     compile_time)
                     .count()
              << "ms, Build time: "
              << std::chrono::duration_cast<std::chrono::milliseconds>(
                     build_output_time)
                     .count()
              << "ms)" << std::endl;
  }

  return return_code;
}
