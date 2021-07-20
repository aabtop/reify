#ifndef _HYPO_CGAL_EMBED_2D_IN_3D_H_
#define _HYPO_CGAL_EMBED_2D_IN_3D_H_

#include <CGAL/Constrained_Delaunay_triangulation_2.h>
#include <CGAL/Triangulation_face_base_with_info_2.h>

#include "cgal/types_nef_polyhedron_3.h"
#include "cgal/types_polygons.h"
#include "cgal/types_surface_mesh.h"
#include "reify/purecpp/hypo.h"

namespace hypo {
namespace cgal {

struct FaceInfo2 {
  int nesting_level;

  bool in_domain() const { return nesting_level % 2 == 1; }
};
using Triangulation_vertex_base_2 = CGAL::Triangulation_vertex_base_2<Kernel>;
using Triangulation_face_base_with_info_2 =
    CGAL::Triangulation_face_base_with_info_2<FaceInfo2, Kernel>;
using Constrained_triangulation_face_base_2 =
    CGAL::Constrained_triangulation_face_base_2<
        Kernel, Triangulation_face_base_with_info_2>;
using Triangulation_data_structure_2 =
    CGAL::Triangulation_data_structure_2<Triangulation_vertex_base_2,
                                         Constrained_triangulation_face_base_2>;
using Constrained_Delaunay_triangulation_2 =
    CGAL::Constrained_Delaunay_triangulation_2<Kernel,
                                               Triangulation_data_structure_2>;

Constrained_Delaunay_triangulation_2 TriangulatePolygonSet(
    const Polygon_set_2& polygon_set);
Nef_polyhedron_3 EmbedPolygonSetAs3DSurfaceMesh(
    const Polygon_set_2& polygon_set,
    const std::vector<hypo::Matrix43>& transforms, bool closed);

}  // namespace cgal
}  // namespace hypo

#endif  // _HYPO_CGAL_EMBED_2D_IN_3D_H_
