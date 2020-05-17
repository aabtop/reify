#ifndef _HYPO_EXPORT_TO_STL_H_
#define _HYPO_EXPORT_TO_STL_H_

#include "cgal/types_nef_polyhedron_3.h"

namespace hypo {
namespace cgal {

bool ExportToSTL(const Nef_polyhedron_3& polyhedron,
                 const std::string& output_filepath);

}  // namespace cgal
}  // namespace hypo

#endif  // _HYPO_EXPORT_TO_STL_H_
