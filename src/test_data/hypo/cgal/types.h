#ifndef _HYPO_CGAL_TYPES_H_
#define _HYPO_CGAL_TYPES_H_

#include <CGAL/Nef_polyhedron_3.h>
#include <CGAL/Polygon_set_2.h>
#include <CGAL/Simple_cartesian.h>

namespace hypo {
namespace cgal {

using Kernel = CGAL::Simple_cartesian<float>;

using Polygon_2 = CGAL::Polygon_2<Kernel>;
using Polygon_with_holes_2 = CGAL::Polygon_with_holes_2<Kernel>;
using Polygon_set_2 = CGAL::Polygon_set_2<Kernel>;
using Point_2 = Kernel::Point_2;
using Vector_2 = Kernel::Vector_2;
using Point_3 = Kernel::Point_3;
using Vector_3 = Kernel::Vector_3;
using Nef_polyhedron_3 = CGAL::Nef_polyhedron_3<Kernel>;

}  // namespace cgal
}  // namespace hypo

#endif  // _HYPO_CGAL_TYPES_H_
