#include <fstream>
#include <iostream>
#include <sstream>

#include "cgal/construct_region2.h"
#include "cgal/construct_region3.h"
#include "cgal/export_to_stl.h"
#include "cgal/export_to_svg.h"
#include "hypo.h"
#include "reify.h"

namespace {
std::string LoadFile(const char* filename) {
  std::ifstream in(filename);
  std::stringstream buffer;
  buffer << in.rdbuf();
  return buffer.str();
}
}  // namespace

int main(int argc, char* argv[]) {
  if (argc < 3) {
    std::cerr << "USAGE: " << argv[0] << " INPUT_FILE" << std::endl;
    return 1;
  }

  hypo::reify::CompilerEnvironment compile_env;
  auto module_or_error = compile_env.Compile(argv[1], LoadFile(argv[1]));
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
      runtime_env->GetExport<hypo::reify::Function<hypo::Region2()>>("Test");
  if (auto error = std::get_if<0>(&entrypoint_or_error)) {
    std::cerr << "Problem finding entrypoint function: " << error << std::endl;
    return 1;
  }
  auto entrypoint = &std::get<1>(entrypoint_or_error);

  auto result_or_error = entrypoint->Call();
  if (auto error = std::get_if<0>(&result_or_error)) {
    std::cerr << "Error running function: " << error << std::endl;
    return 1;
  }

  hypo::cgal::ExportToSVG(
      *hypo::cgal::ConstructRegion2(std::get<1>(result_or_error)), argv[2]);

  /*
  hypo::cgal::ExportToSTL(
      *hypo::cgal::ConstructRegion3(std::get<1>(result_or_error)), argv[2]);
  */

  return 0;
}
