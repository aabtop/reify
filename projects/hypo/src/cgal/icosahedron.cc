#include "cgal/icosahedron.h"

#include <CGAL/Polygon_mesh_processing/polygon_soup_to_polygon_mesh.h>

#include <array>

namespace hypo {
namespace cgal {

namespace {
const std::array<Point_3, 12> points = {{
    {-0.5257310271, 0.0000000000, -0.8506510258},
    {0.5257310271, 0.0000000000, -0.8506510258},
    {0.5257310271, 0.0000000000, 0.8506510258},
    {-0.5257310271, 0.0000000000, 0.8506510258},
    {-0.8506510258, -0.5257310271, 0.0000000000},
    {-0.8506510258, 0.5257310271, 0.0000000000},
    {0.8506510258, 0.5257310271, 0.0000000000},
    {0.8506510258, -0.5257310271, 0.0000000000},
    {0.0000000000, -0.8506510258, 0.5257310271},
    {0.0000000000, -0.8506510258, -0.5257310271},
    {0.0000000000, 0.8506510258, -0.5257310271},
    {0.0000000000, 0.8506510258, 0.5257310271},
}};

using Triangle = std::array<std::size_t, 3>;
const std::array<Triangle, 20> triangles = {{
    {1, 9, 0},   {10, 1, 0},  {5, 10, 0}, {4, 5, 0},  {9, 4, 0},
    {8, 2, 3},   {4, 8, 3},   {5, 4, 3},  {11, 5, 3}, {2, 11, 3},
    {11, 2, 6},  {10, 11, 6}, {1, 10, 6}, {7, 1, 6},  {2, 7, 6},
    {11, 10, 5}, {9, 8, 4},   {7, 2, 8},  {9, 7, 8},  {1, 7, 9},
}};
}  // namespace

Surface_mesh MakeUnitIcosahedronMesh() {
  Surface_mesh mesh;
  CGAL::Polygon_mesh_processing::polygon_soup_to_polygon_mesh(points, triangles,
                                                              mesh);
  return mesh;
}

}  // namespace cgal
}  // namespace hypo
