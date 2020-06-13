#ifndef _HYPO_CGAL_CONVERSION_UTILS_H_
#define _HYPO_CGAL_CONVERSION_UTILS_H_

#include "cgal/types_core.h"
#include "hypo.h"

namespace hypo {
namespace cgal {

inline Aff_transformation_2 ToAff_transformation_2(
    const hypo::Matrix33& matrix) {
  return Aff_transformation_2(matrix[0], matrix[1], matrix[2], matrix[3],
                              matrix[4], matrix[5], matrix[8]);
}

inline Aff_transformation_3 ToAff_transformation_3(
    const hypo::Matrix44& matrix) {
  return Aff_transformation_3(matrix[0], matrix[1], matrix[2], matrix[3],
                              matrix[4], matrix[5], matrix[6], matrix[7],
                              matrix[8], matrix[9], matrix[10], matrix[11],
                              matrix[15]);
}

}  // namespace cgal
}  // namespace hypo

#endif  // _HYPO_CGAL_CONVERSION_UTILS_H_
