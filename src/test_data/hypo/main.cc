#include <fstream>
#include <iostream>
#include <sstream>

#include "cgal/construct_mesh2.h"
#include "cgal/construct_mesh3.h"
#include "cgal/embed_2d_in_3d.h"
#include "cgal/export_to_stl.h"
#include "hypo.h"
#include "reify.h"

namespace {
std::string LoadFile(const char* filename) {
  std::ifstream in(filename);
  std::stringstream buffer;
  buffer << in.rdbuf();
  return buffer.str();
}

void ProcessResult(const hypo::Mesh3& mesh3) {
  if (auto extrude = std::get_if<std::shared_ptr<hypo::ExtrudeMesh2>>(&mesh3)) {
    std::cout << "ExtrudeMesh2AsMesh" << std::endl;
  } else if (auto transform =
                 std::get_if<std::shared_ptr<hypo::TransformMesh3>>(&mesh3)) {
    std::cout << "TransformMesh3AsMesh" << std::endl;
  } else if (auto mesh_union =
                 std::get_if<std::shared_ptr<hypo::Mesh3Union>>(&mesh3)) {
    std::cout << "MeshUnion" << std::endl;
    std::cout << "  Number of meshes: " << (*mesh_union)->meshes.size()
              << std::endl;
    std::cout << "  [" << std::endl;
    for (const auto& mesh : (*mesh_union)->meshes) {
      ProcessResult(mesh);
    }
    std::cout << "  ]" << std::endl;
  }
}
}  // namespace

int main(int argc, char* argv[]) {
  if (argc < 2) {
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
      runtime_env->GetExport<hypo::reify::Function<hypo::Mesh2()>>("Test");
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

  hypo::cgal::ExportToSTL(hypo::cgal::EmbedPolygonSetIn3DXYPlane(
      *hypo::cgal::ConstructMesh2(std::get<1>(result_or_error))));

  return 0;
}
