#ifndef _HYPO_CGAL_CONSTRUCT_MESH3_H_
#define _HYPO_CGAL_CONSTRUCT_MESH3_H_

#include <memory>

#include "cgal/types.h"
#include "hypo.h"

namespace hypo {
namespace cgal {

std::shared_ptr<Polygon_set_2> ConstructMesh2(const hypo::Mesh2& mesh2);

}  // namespace cgal
}  // namespace hypo

#endif  // _HYPO_CGAL_CONSTRUCT_MESH3_H_
