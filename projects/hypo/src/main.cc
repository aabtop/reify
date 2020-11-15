#include <chrono>
#include <filesystem>
#include <fstream>
#include <iostream>
#include <optional>

#include "CLI/CLI.hpp"
#include "cgal/export_to_file.h"
#include "reify/purecpp/hypo.h"
#include "reify/typescript_cpp_v8.h"
#include "reify/typescript_cpp_v8/command_line_tool.h"
#include "reify/typescript_cpp_v8/hypo.h"

using namespace std::chrono;

namespace {
struct CallAndExportResults {
  std::chrono::microseconds call_time;
  std::chrono::microseconds build_time;
  std::chrono::microseconds export_time;
  std::string output_filepath;
};

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

}  // namespace

int main(int argc, char* argv[]) {
  std::filesystem::path output_file_basepath;

  auto maybe_result = reify::typescript_cpp_v8::CommandLineToolParse(
      reify::typescript_cpp_v8::CommandLineToolParameters{
          "hypo", "Build geometry declaritively with TypeScript.",
          reify::typescript_cpp_v8::hypo::typescript_declarations(),
          [&output_file_basepath](CLI::App* app) {
            app->add_option(
                   "output_file_basepath,-o,--output_file_basepath",
                   output_file_basepath,
                   "The output file path, without an extension, where the "
                   "output "
                   "will be generated into.  The extension will be chosen "
                   "based "
                   "on the return type of the specified input function.")
                ->required();
          }},
      argc, argv);

  if (int* exit_code = std::get_if<int>(&maybe_result)) {
    return *exit_code;
  }
  auto results = std::move(std::get<1>(maybe_result));

  auto build_results = BuildOutputAndSaveToFile(&(*results->runtime_env),
                                                results->entry_point_symbol,
                                                output_file_basepath);

  if (!build_results) {
    return 1;
  }

  PrintResultsInformation(std::cerr, results->compile_time, *build_results);

  return 0;
}
