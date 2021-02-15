#include "src/ide/domain_visualizer_hypo.h"

#include <CGAL/boost/graph/convert_nef_polyhedron_to_polygon_mesh.h>

#include "cgal/construct_region2.h"
#include "cgal/construct_region3.h"
#include "cgal/types_nef_polyhedron_3.h"
#include "reify/purecpp/hypo.h"
#include "reify/typescript_cpp_v8.h"
#include "reify/typescript_cpp_v8/hypo.h"

namespace {
TriangleSoup ConvertToTriangleSoup(
    const hypo::cgal::Nef_polyhedron_3& polyhedron) {
  std::vector<hypo::cgal::Point_3> cgal_vertices;
  std::vector<std::vector<size_t>> cgal_faces;
  CGAL::convert_nef_polyhedron_to_polygon_soup(polyhedron, cgal_vertices,
                                               cgal_faces, true);

  std::vector<TriangleSoup::Triangle> triangles;
  triangles.reserve(cgal_faces.size());
  std::vector<TriangleSoup::Vertex> vertices;
  vertices.reserve(triangles.size() * 3 * sizeof(TriangleSoup::Vertex));

  for (const auto& cgal_face : cgal_faces) {
    assert(cgal_face.size() == 3);
    const std::array<hypo::cgal::Point_3, 3> points = {
        cgal_vertices[cgal_face[0]], cgal_vertices[cgal_face[1]],
        cgal_vertices[cgal_face[2]]};
    hypo::cgal::Vector_3 cgal_normal =
        CGAL::normal(points[0], points[1], points[2]);

    TriangleSoup::Vector3 normal = {
        static_cast<float>(CGAL::to_double(cgal_normal.x())),
        static_cast<float>(CGAL::to_double(cgal_normal.y())),
        static_cast<float>(CGAL::to_double(cgal_normal.z()))};

    for (const auto& point : points) {
      vertices.push_back(TriangleSoup::Vertex{
          {static_cast<float>(CGAL::to_double(point.x())),
           static_cast<float>(CGAL::to_double(point.y())),
           static_cast<float>(CGAL::to_double(point.z()))},
          normal,
      });
    }

    triangles.push_back(
        TriangleSoup::Triangle{static_cast<uint32_t>(vertices.size() - 3),
                               static_cast<uint32_t>(vertices.size() - 2),
                               static_cast<uint32_t>(vertices.size() - 1)});
  }

  return TriangleSoup{std::move(vertices), std::move(triangles)};
}
}  // namespace

DomainVisualizerHypo::DomainVisualizerHypo(
    const std::function<void(TriangleSoup&&)>& produce_mesh)
    : produce_mesh_(produce_mesh) {}

std::vector<reify::CompilerEnvironment::InputModule>
DomainVisualizerHypo::GetTypeScriptModules() {
  return reify::typescript_cpp_v8::hypo::typescript_declarations();
}

bool DomainVisualizerHypo::CanConsumeSymbol(
    const reify::CompiledModule::ExportedSymbol& symbol) {
  return (/*symbol.HasType<reify::Function<hypo::Region2()>>() ||*/
          symbol.HasType<reify::Function<hypo::Region3()>>());
}

void DomainVisualizerHypo::ConsumeSymbol(
    std::shared_ptr<reify::CompiledModule> module,
    const reify::CompiledModule::ExportedSymbol& symbol,
    const std::function<void(std::optional<ConsumeError>&&)>& on_consumed) {
  std::thread thread([module, symbol = std::move(symbol),
                      on_consumed = std::move(on_consumed),
                      &produce_mesh = produce_mesh_]() {
    // Setup a V8 runtime environment around the CompiledModule.  This will
    // enable us to call exported functions and query exported values from the
    // module.
    auto runtime_env_or_error = reify::CreateRuntimeEnvironment(module);
    if (auto error = std::get_if<0>(&runtime_env_or_error)) {
      on_consumed(*error);
      return;
    }

    reify::RuntimeEnvironment runtime_env(
        std::move(std::get<1>(runtime_env_or_error)));

    if (symbol.HasType<reify::Function<hypo::Region3()>>()) {
      auto entry_point_or_error =
          runtime_env.GetExport<reify::Function<hypo::Region3()>>(symbol.name);
      if (auto error = std::get_if<0>(&entry_point_or_error)) {
        on_consumed("Problem finding entrypoint function: " + *error);
        return;
      }
      auto entry_point = &std::get<1>(entry_point_or_error);

      auto result_or_error = entry_point->Call();
      if (auto error = std::get_if<0>(&result_or_error)) {
        on_consumed("Error running function: " + *error);
        return;
      }

      hypo::Region3 result = std::get<1>(result_or_error);

      hypo::cgal::Nef_polyhedron_3 polyhedron3 =
          hypo::cgal::ConstructRegion3(result);

      produce_mesh(ConvertToTriangleSoup(polyhedron3));

      on_consumed(std::nullopt);
    }
  });

  thread.detach();  // TODO: Manage the threading framework better.
}
