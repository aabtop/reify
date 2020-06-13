#ifndef _HYPO_CGAL_TYPES_CORE_H_
#define _HYPO_CGAL_TYPES_CORE_H_

#include <CGAL/Aff_transformation_3.h>
#include <CGAL/Exact_predicates_exact_constructions_kernel.h>
#include <CGAL/Simple_cartesian.h>

namespace hypo {
namespace cgal {

// If we use CGAL::Simple_cartesian<float>, we seem to encounter a bunch of
// bugs in CGAL, for example when trying to generate a Polygon_2 of a segmented
// circle.
using Kernel = CGAL::Exact_predicates_exact_constructions_kernel;

using Point_2 = Kernel::Point_2;
using Vector_2 = Kernel::Vector_2;
using Point_3 = Kernel::Point_3;
using Vector_3 = Kernel::Vector_3;
using Aff_transformation_2 = CGAL::Aff_transformation_2<Kernel>;
using Aff_transformation_3 = CGAL::Aff_transformation_3<Kernel>;

const float kPi = 3.14159265359f;

}  // namespace cgal
}  // namespace hypo

#endif  // _HYPO_CGAL_TYPES_CORE_H_
