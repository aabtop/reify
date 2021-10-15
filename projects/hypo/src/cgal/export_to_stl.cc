#include "cgal/export_to_stl.h"

#include <CGAL/Polygon_mesh_processing/polygon_mesh_to_polygon_soup.h>

#include <filesystem>
#include <fstream>

#include "cgal/types_nef_polyhedron_3.h"

namespace hypo {
namespace cgal {

bool ExportToSTL(const Surface_mesh& mesh,
                 const std::filesystem::path& output_filepath) {
  std::vector<Point_3> vertices;
  std::vector<std::vector<size_t>> faces;
  CGAL::Polygon_mesh_processing::polygon_mesh_to_polygon_soup(mesh, vertices,
                                                              faces);

  std::ostringstream out;

  out << "solid " << output_filepath.string() << std::endl;
  for (const auto& face : faces) {
    const Point_3& point1 = vertices[face[0]];
    const Point_3& point2 = vertices[face[1]];
    const Point_3& point3 = vertices[face[2]];
    Vector_3 normal = CGAL::normal(point1, point2, point3);

    out << "  facet normal " << normal.x() << " " << normal.y() << " "
        << normal.z() << std::endl;
    out << "    outer loop" << std::endl;
    auto output_vertex = [&out](const Point_3& v) {
      out << "      vertex " << v.x() << " " << v.y() << " " << v.z()
          << std::endl;
    };
    output_vertex(point1);
    output_vertex(point2);
    output_vertex(point3);
    out << "    endloop" << std::endl;
    out << "  endfacet" << std::endl;
  }
  out << "endsolid " << output_filepath.string() << std::endl;

  std::ofstream fout(output_filepath);
  auto out_string = out.str();
  fout.write(out_string.c_str(), out_string.size());

  return true;
}

}  // namespace cgal
}  // namespace hypo
