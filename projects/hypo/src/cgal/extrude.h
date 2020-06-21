#ifndef _HYPO_CGAL_EXTRUDE_H_
#define _HYPO_CGAL_EXTRUDE_H_

#include <vector>

#include "cgal/types_surface_mesh.h"
#include "hypo.h"

namespace hypo {
namespace cgal {

void ExtrudeMeshWithTransformList(const Surface_mesh& input,
                                  Surface_mesh& output,
                                  const std::vector<Matrix43>& transforms,
                                  bool closed);

}  // namespace cgal
}  // namespace hypo

#endif  // _HYPO_CGAL_EXTRUDE_H_
