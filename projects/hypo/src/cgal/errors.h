#ifndef _HYPO_CGAL_ERRORS_H_
#define _HYPO_CGAL_ERRORS_H_

#include <CGAL/exceptions.h>
#include <fmt/format.h>

#include "reify/utils/error.h"

namespace hypo {
namespace cgal {

inline reify::utils::Error CgalExceptionToError(
    const CGAL::Failure_exception& e) {
  if (e.message().empty()) {
    return reify::utils::Error{
        fmt::format("CGAL error on assertion: {}", e.expression())};
  } else {
    return reify::utils::Error{fmt::format("CGAL error: {} (on assertion: {})",
                                           e.message(), e.expression())};
  }
}

template <typename T, typename X>
auto CallCgalAndCatchExceptions(const T& function, const X& x)
    -> reify::utils::ErrorOr<typename std::result_of<T(const X&)>::type> {
  try {
    return function(x);
  } catch (const CGAL::Failure_exception& e) {
    return CgalExceptionToError(e);
  }
}

}  // namespace cgal
}  // namespace hypo

#endif  // _HYPO_CGAL_ERRORS_H_
