#include "hypo/geometry/export_to_stl.h"

#include <filesystem>
#include <fstream>
#include <glm/gtc/matrix_inverse.hpp>
#include <sstream>

#include "hypo/geometry/triangle_soup.h"

namespace hypo {
namespace geometry {

bool ExportToSTL(const hypo::geometry::TriangleSoupSet& triangle_soup_set,
                 const std::filesystem::path& output_filepath) {
  std::ostringstream out;

  out << "solid " << output_filepath.string() << std::endl;
  for (const auto& triangle_soup : triangle_soup_set) {
    for (const auto& triangle : *triangle_soup->triangles) {
      const TriangleSoup::Vertex& v1 = (*triangle_soup->vertices)[triangle[0]];
      const TriangleSoup::Vertex& v2 = (*triangle_soup->vertices)[triangle[1]];
      const TriangleSoup::Vertex& v3 = (*triangle_soup->vertices)[triangle[2]];

      // Might be better to just compute the normal here, but at the time at
      // least it happens to be the case that all vertices share the Triangle's
      // normal.
      glm::mat4 normal_transform =
          glm::inverseTranspose(triangle_soup->transform);
      glm::vec4 transformed_normal =
          normal_transform * glm::vec4(v1.normal, 0.0f);
      out << "  facet normal " << transformed_normal[0] << " "
          << transformed_normal[1] << " " << transformed_normal[2] << std::endl;
      out << "    outer loop" << std::endl;
      auto output_vertex = [&out,
                            &triangle_soup](const TriangleSoup::Vertex& v) {
        glm::vec4 transformed_position =
            triangle_soup->transform * glm::vec4(v.position, 1.0f);
        out << "      vertex " << transformed_position[0] << " "
            << transformed_position[1] << " " << transformed_position[2]
            << std::endl;
      };
      output_vertex(v1);
      output_vertex(v2);
      output_vertex(v3);
      out << "    endloop" << std::endl;
      out << "  endfacet" << std::endl;
    }
  }
  out << "endsolid " << output_filepath.string() << std::endl;

  std::ofstream fout(output_filepath);
  auto out_string = out.str();
  fout.write(out_string.c_str(), out_string.size());

  return true;
}

}  // namespace geometry
}  // namespace hypo
