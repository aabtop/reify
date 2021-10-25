#include "cgal/construct_trianglesoup3.h"

#include <CGAL/Polygon_mesh_processing/polygon_mesh_to_polygon_soup.h>
#include <CGAL/Polygon_mesh_processing/triangulate_faces.h>
#include <CGAL/boost/graph/convert_nef_polyhedron_to_polygon_mesh.h>

#include <glm/gtc/type_ptr.hpp>

#include "cgal/construct_mesh3.h"
#include "cgal/construct_region3.h"
#include "cgal/conversions.h"
#include "cgal/types_nef_polyhedron_3.h"
#include "cgal/types_surface_mesh.h"
#include "reify/pure_cpp/cache.h"
#include "reify/purecpp/hypo.h"

namespace reify {
namespace pure_cpp {

template <>
int64_t EstimatedMemoryUsageInBytes(const ::hypo::cgal::TriangleSoup& x) {
  // Empirically discovered by recording before/after memory usage, but probably
  // wrong.
  return x.vertices->size() * sizeof((*x.vertices)[0]) +
         x.triangles->size() * sizeof((*x.triangles)[0]) + sizeof(x);
}

template <>
int64_t EstimatedMemoryUsageInBytes(const ::hypo::cgal::TriangleSoupSet& x) {
  // Empirically discovered by recording before/after memory usage, but probably
  // wrong.
  return x.size() * sizeof(**x.begin());
}

}  // namespace pure_cpp
}  // namespace reify

namespace hypo {
namespace cgal {

namespace {
TriangleSoup TriangleSoupForVerticesAndTriangles(
    const std::vector<hypo::cgal::Point_3>& cgal_vertices,
    const std::vector<std::vector<size_t>>& cgal_triangles) {
  std::vector<TriangleSoup::Vertex> vertices;
  vertices.reserve(cgal_vertices.size());
  std::vector<TriangleSoup::Triangle> triangles;
  triangles.reserve(cgal_triangles.size());

  for (const auto& cgal_triangle : cgal_triangles) {
    assert(cgal_triangle.size() == 3);
    const std::array<const hypo::cgal::Point_3, 3> points = {
        cgal_vertices[cgal_triangle[0]], cgal_vertices[cgal_triangle[1]],
        cgal_vertices[cgal_triangle[2]]};
    const hypo::cgal::Vector_3 cgal_normal =
        CGAL::normal(points[0], points[1], points[2]);
    TriangleSoup::Vector3 normal = {
        static_cast<float>(CGAL::to_double(cgal_normal.x())),
        static_cast<float>(CGAL::to_double(cgal_normal.y())),
        static_cast<float>(CGAL::to_double(cgal_normal.z()))};

    for (const auto& point : points) {
      vertices.push_back(TriangleSoup::Vertex{
          TriangleSoup::Vector3{static_cast<float>(CGAL::to_double(point.x())),
                                static_cast<float>(CGAL::to_double(point.y())),
                                static_cast<float>(CGAL::to_double(point.z()))},
          normal});
    }

    triangles.push_back(
        TriangleSoup::Triangle{static_cast<uint32_t>(vertices.size() - 3),
                               static_cast<uint32_t>(vertices.size() - 2),
                               static_cast<uint32_t>(vertices.size() - 1)});
  }

  return TriangleSoup{
      std::make_shared<const std::vector<TriangleSoup::Vertex>>(
          std::move(vertices)),
      std::make_shared<const std::vector<TriangleSoup::Triangle>>(
          std::move(triangles)),
      {1.0f, 1.0f, 1.0f},  // Color
      {1.0f, 0.0f, 0.0f, 0.0f, 0.0f, 1.0f, 0.0f, 0.0f, 0.0f, 0.0f, 1.0f, 0.0f,
       0.0f, 0.0f, 0.0f, 1.0f}  // Identity transform
  };
};
}  // namespace

TriangleSoup ConstructTriangleSoup3(
    reify::pure_cpp::ThreadPoolCacheRunner* runner,
    const hypo::TriangleSoupFromMesh3& x) {
  auto mesh = *hypo::cgal::ConstructMesh3(runner, x.mesh).Get();

  std::vector<hypo::cgal::Point_3> cgal_vertices;
  std::vector<std::vector<size_t>> cgal_triangles;
  hypo::cgal::Surface_mesh triangulated_mesh = mesh;
  CGAL::Polygon_mesh_processing::triangulate_faces(triangulated_mesh);
  CGAL::Polygon_mesh_processing::polygon_mesh_to_polygon_soup(
      triangulated_mesh, cgal_vertices, cgal_triangles);

  return TriangleSoupForVerticesAndTriangles(cgal_vertices, cgal_triangles);
}

TriangleSoup ConstructTriangleSoup3(
    reify::pure_cpp::ThreadPoolCacheRunner* runner,
    const hypo::TriangleSoupFromRegion3& x) {
  auto polyhedron = *hypo::cgal::ConstructRegion3(runner, x.region).Get();

  std::vector<hypo::cgal::Point_3> cgal_vertices;
  std::vector<std::vector<size_t>> cgal_triangles;
  CGAL::convert_nef_polyhedron_to_polygon_soup(polyhedron, cgal_vertices,
                                               cgal_triangles,
                                               true  // trianglue_all_faces
  );

  return TriangleSoupForVerticesAndTriangles(cgal_vertices, cgal_triangles);
}

TriangleSoup ConstructTriangleSoup3(
    reify::pure_cpp::ThreadPoolCacheRunner* runner,
    const hypo::AffineTransformTriangleSoup3& x) {
  auto previous_triangle_soup =
      *hypo::cgal::ConstructTriangleSoup3(runner, x.triangle_soup).Get();

  return TriangleSoup{
      previous_triangle_soup.vertices, previous_triangle_soup.triangles,
      previous_triangle_soup.color,
      glm::make_mat4(x.transform.data()) * previous_triangle_soup.transform};
}

TriangleSoup ConstructTriangleSoup3(
    reify::pure_cpp::ThreadPoolCacheRunner* runner,
    const hypo::TriangleSoupWithColor3& x) {
  auto previous_triangle_soup =
      *hypo::cgal::ConstructTriangleSoup3(runner, x.triangle_soup).Get();

  return TriangleSoup{previous_triangle_soup.vertices,
                      previous_triangle_soup.triangles, x.color,
                      previous_triangle_soup.transform};
}

TriangleSoupSet _ConstructTriangleSoupSet3(
    reify::pure_cpp::ThreadPoolCacheRunner* runner,
    const hypo::TriangleSoupSet3& x) {
  std::vector<FutureTriangleSoup3> children;
  children.reserve(x.triangle_soups.size());
  for (const auto& triangle_soup : x.triangle_soups) {
    children.push_back(ConstructTriangleSoup3(runner, triangle_soup));
  }

  TriangleSoupSet result;
  for (auto& child : children) {
    result.insert(child.Get());
  }
  return result;
}  // namespace

FutureTriangleSoup3 ConstructTriangleSoup3(
    reify::pure_cpp::ThreadPoolCacheRunner* runner,
    const hypo::TriangleSoup3& x) {
  return std::visit(
      [runner](const auto& y) {
        return runner->MakeFuture<TriangleSoup>(&ConstructTriangleSoup3, y);
      },
      static_cast<const hypo::TriangleSoup3::AsVariant&>(x));
}

FutureTriangleSoupSet3 ConstructTriangleSoupSet3(
    reify::pure_cpp::ThreadPoolCacheRunner* runner,
    const hypo::TriangleSoupSet3& x) {
  return runner->MakeFuture<TriangleSoupSet>(&_ConstructTriangleSoupSet3, x);
}

}  // namespace cgal
}  // namespace hypo