#ifndef _HYPO_GEOMETRY_EXPORT_TO_STL_H_
#define _HYPO_GEOMETRY_EXPORT_TO_STL_H_

#include <filesystem>

#include "hypo/geometry/triangle_soup.h"

namespace hypo {
namespace geometry {

bool ExportToSTL(const hypo::geometry::TriangleSoupSet& triangle_soup_set,
                 const std::filesystem::path& output_filepath);

}  // namespace geometry
}  // namespace hypo

#endif  // _HYPO_GEOMETRY_EXPORT_TO_STL_H_
