#ifndef _HYPO_EXPORT_TO_SVG_H_
#define _HYPO_EXPORT_TO_SVG_H_

#include "cgal/types_polygons.h"

namespace hypo {
namespace cgal {

bool ExportToSVG(const Polygon_set_2& polygon_set,
                 const std::string& output_filepath);

}  // namespace cgal
}  // namespace hypo

#endif  // _HYPO_EXPORT_TO_SVG_H_
