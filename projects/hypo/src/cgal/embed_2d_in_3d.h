#ifndef _HYPO_CGAL_EMBED_2D_IN_3D_H_
#define _HYPO_CGAL_EMBED_2D_IN_3D_H_

#include "cgal/types.h"

namespace hypo {
namespace cgal {

Nef_polyhedron_3 EmbedPolygonSetIn3DXYPlane(const Polygon_set_2& polygon_set);

Nef_polyhedron_3 EmbedPolygonSetAs3DSurfaceMesh(
    const Polygon_set_2& polygon_set);

}  // namespace cgal
}  // namespace hypo

#endif  // _HYPO_CGAL_EMBED_2D_IN_3D_H_
