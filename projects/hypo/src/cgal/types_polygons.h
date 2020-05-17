#ifndef _HYPO_CGAL_TYPES_POLYGONS_H_
#define _HYPO_CGAL_TYPES_POLYGONS_H_

#include <CGAL/Bbox_2.h>
#include <CGAL/Polygon_set_2.h>

#include "cgal/types_core.h"

namespace hypo {
namespace cgal {

using Polygon_2 = CGAL::Polygon_2<Kernel>;
using Polygon_with_holes_2 = CGAL::Polygon_with_holes_2<Kernel>;
using Polygon_set_2 = CGAL::Polygon_set_2<Kernel>;
using Bbox_2 = CGAL::Bbox_2;

}  // namespace cgal
}  // namespace hypo

#endif  // _HYPO_CGAL_TYPES_POLYGONS_H_
