#ifndef _HYPO_CGAL_CONSTRUCT_MESH3_H_
#define _HYPO_CGAL_CONSTRUCT_MESH3_H_

#include <memory>

#include "cgal/types.h"
#include "hypo.h"

namespace hypo {
namespace cgal {

std::shared_ptr<Nef_polyhedron_3> ConstructMesh3(const hypo::Mesh3& mesh3);

}  // namespace cgal
}  // namespace hypo

#endif  // _HYPO_CGAL_CONSTRUCT_MESH3_H_
