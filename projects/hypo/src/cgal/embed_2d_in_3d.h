#ifndef _HYPO_CGAL_EMBED_2D_IN_3D_H_
#define _HYPO_CGAL_EMBED_2D_IN_3D_H_

#include "cgal/types_nef_polyhedron_3.h"
#include "cgal/types_polygons.h"
#include "reify/purecpp/hypo.h"

namespace hypo {
namespace cgal {

Nef_polyhedron_3 EmbedPolygonSetAs3DSurfaceMesh(
    const Polygon_set_2& polygon_set,
    const std::vector<hypo::Matrix43>& transforms, bool closed);

}  // namespace cgal
}  // namespace hypo

#endif  // _HYPO_CGAL_EMBED_2D_IN_3D_H_
