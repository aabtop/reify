#ifndef _HYPO_CGAL_TYPES_CORE_H_
#define _HYPO_CGAL_TYPES_CORE_H_

#include <CGAL/Aff_transformation_3.h>
#include <CGAL/Cartesian.h>
#include <CGAL/Exact_predicates_exact_constructions_kernel.h>
#include <CGAL/Exact_predicates_inexact_constructions_kernel.h>
#include <CGAL/Simple_cartesian.h>

namespace hypo {
namespace cgal {

// Note that Exact_predicates_exact_constructions_kernel is *not* thread-safe.
// See
// https://stackoverflow.com/questions/68086180/problem-with-multi-threaded-construction-of-nef-polyhedrons-from-a-same-polyhedr
//   - A fix was merged into master on July 27th:
//   https://github.com/CGAL/cgal/pull/5402
// For this reason we're using an internal release of CGAL.
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
