#include <fstream>
#include <iostream>
#include <optional>
#include <sstream>

#include "cgal/export_to_file.h"
#include "hypo.h"
#include "reify.h"

namespace {
std::optional<std::string> LoadFile(const char* filename) {
  std::ifstream in(filename);
  if (in.fail()) return std::nullopt;

  std::stringstream buffer;
  buffer << in.rdbuf();
  return buffer.str();
}
}  // namespace

int main(int argc, char* argv[]) {
  if (argc < 3) {
    std::cerr << "USAGE: " << argv[0]
              << " INPUT_FILE OUTPUT_FILE_WITHOUT_EXTENSION" << std::endl;
    return 1;
  }

  auto input_file = LoadFile(argv[1]);
  if (!input_file) {
    std::cerr << "Error loading file '" << argv[1] << "'." << std::endl;
    return 1;
  }

  hypo::reify::CompilerEnvironment compile_env;
  auto module_or_error = compile_env.Compile(argv[1], *input_file);
  if (auto error = std::get_if<0>(&module_or_error)) {
    std::cerr << "Error compiling TypeScript:" << std::endl;
    std::cerr << error->path << ":" << error->line + 1 << ":"
              << error->column + 1 << ": error: " << error->message
              << std::endl;
    return 1;
  }
  auto module = std::get<1>(module_or_error);

  auto runtime_env_or_error = CreateRuntimeEnvironment(module);
  if (auto error = std::get_if<0>(&runtime_env_or_error)) {
    std::cerr << "Error initializing module: " << std::endl;
    std::cerr << error << std::endl;
    return 1;
  }
  auto runtime_env = &std::get<1>(runtime_env_or_error);

  auto entrypoint_or_error =
      runtime_env->GetExport<hypo::reify::Function<hypo::Region3()>>("Jeep");
  if (auto error = std::get_if<0>(&entrypoint_or_error)) {
    std::cerr << "Problem finding entrypoint function: " << *error << std::endl;
    return 1;
  }
  auto entrypoint = &std::get<1>(entrypoint_or_error);

  auto result_or_error = entrypoint->Call();
  if (auto error = std::get_if<0>(&result_or_error)) {
    std::cerr << "Error running function: " << error << std::endl;
    return 1;
  }

  hypo::cgal::ExportToFile(std::get<1>(result_or_error), argv[2]);

  return 0;
}
