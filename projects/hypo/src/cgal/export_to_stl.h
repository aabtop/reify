#ifndef _HYPO_EXPORT_TO_STL_H_
#define _HYPO_EXPORT_TO_STL_H_

#include <filesystem>

#include "cgal/types_surface_mesh.h"

namespace hypo {
namespace cgal {

bool ExportToSTL(const Surface_mesh& mesh,
                 const std::filesystem::path& output_filepath);

}  // namespace cgal
}  // namespace hypo

#endif  // _HYPO_EXPORT_TO_STL_H_
