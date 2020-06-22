#include "cgal/primitives_3d.h"

#include <CGAL/Polygon_mesh_processing/polygon_soup_to_polygon_mesh.h>

#include <array>

namespace hypo {
namespace cgal {

namespace {
const std::array<Point_3, 12> icosahedron_points = {{
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
const std::array<Triangle, 20> icosahedron_triangles = {{
    {1, 9, 0},   {10, 1, 0},  {5, 10, 0}, {4, 5, 0},  {9, 4, 0},
    {8, 2, 3},   {4, 8, 3},   {5, 4, 3},  {11, 5, 3}, {2, 11, 3},
    {11, 2, 6},  {10, 11, 6}, {1, 10, 6}, {7, 1, 6},  {2, 7, 6},
    {11, 10, 5}, {9, 8, 4},   {7, 2, 8},  {9, 7, 8},  {1, 7, 9},
}};
}  // namespace

Surface_mesh MakeUnitIcosahedronMesh() {
  Surface_mesh mesh;
  CGAL::Polygon_mesh_processing::polygon_soup_to_polygon_mesh(
      icosahedron_points, icosahedron_triangles, mesh);
  return mesh;
}

namespace {
const std::array<Point_3, 6> octahedron_points = {{
    {0.0000000000, 0.0000000000, 1.0000000000},
    {1.0000000000, 0.0000000000, 0.0000000000},
    {0.0000000000, 1.0000000000, 0.0000000000},
    {-1.0000000000, 0.0000000000, 0.0000000000},
    {0.0000000000, -1.0000000000, 0.0000000000},
    {0.0000000000, 0.0000000000, -1.0000000000},
}};

using Triangle = std::array<std::size_t, 3>;
const std::array<Triangle, 8> octahedron_triangles = {{
    {1, 0, 4},
    {4, 0, 3},
    {3, 0, 2},
    {2, 0, 1},
    {1, 5, 2},
    {2, 5, 3},
    {3, 5, 4},
    {4, 5, 1},
}};
}  // namespace

Surface_mesh MakeUnitOctahedronMesh() {
  Surface_mesh mesh;
  CGAL::Polygon_mesh_processing::polygon_soup_to_polygon_mesh(
      octahedron_points, octahedron_triangles, mesh);
  return mesh;
}

namespace {
const std::array<Point_3, 8> box_points = {{
    {0.0000000000, 0.0000000000, 0.0000000000},
    {0.0000000000, 0.0000000000, 1.0000000000},
    {0.0000000000, 1.0000000000, 0.0000000000},
    {0.0000000000, 1.0000000000, 1.0000000000},
    {1.0000000000, 0.0000000000, 0.0000000000},
    {1.0000000000, 0.0000000000, 1.0000000000},
    {1.0000000000, 1.0000000000, 0.0000000000},
    {1.0000000000, 1.0000000000, 1.0000000000},
}};

using Triangle = std::array<std::size_t, 3>;
const std::array<Triangle, 12> box_triangles = {{
    {0, 1, 3},
    {3, 2, 0},
    {6, 7, 5},
    {5, 4, 6},
    {1, 0, 4},
    {4, 5, 1},
    {2, 3, 7},
    {7, 6, 2},
    {0, 2, 6},
    {6, 4, 0},
    {3, 1, 5},
    {5, 7, 3},
}};
}  // namespace

Surface_mesh MakeUnitBoxMesh() {
  Surface_mesh mesh;
  CGAL::Polygon_mesh_processing::polygon_soup_to_polygon_mesh(
      box_points, box_triangles, mesh);
  return mesh;
}

}  // namespace cgal
}  // namespace hypo
