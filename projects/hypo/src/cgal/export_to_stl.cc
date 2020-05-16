#include "cgal/export_to_stl.h"

#include <CGAL/boost/graph/convert_nef_polyhedron_to_polygon_mesh.h>

#include <fstream>

#include "cgal/types.h"

namespace hypo {
namespace cgal {

bool ExportToSTL(const Nef_polyhedron_3& polyhedron,
                 const std::string& output_filepath) {
  std::vector<Point_3> vertices;
  std::vector<std::vector<size_t>> faces;
  CGAL::convert_nef_polyhedron_to_polygon_soup(polyhedron, vertices, faces,
                                               true);

  std::ofstream out(output_filepath);

  out << "solid " << output_filepath << std::endl;
  for (const auto& face : faces) {
    out << "  facet normal " << 0.0f << " " << 0.0f << " " << 0.0f << std::endl;
    out << "    outer loop" << std::endl;
    for (const auto& index : face) {
      const Point_3& point = vertices[index];
      out << "      vertex " << point.x() << " " << point.y() << " "
          << point.z() << std::endl;
    }
    out << "    endloop" << std::endl;
    out << "  endfacet" << std::endl;
  }
  out << "endsolid " << output_filepath << std::endl;

  return true;
}

}  // namespace cgal
}  // namespace hypo
