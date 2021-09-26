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

template <typename T, typename... Args>
auto CallCgalAndCatchExceptions(const T& function, Args&&... params)
    -> reify::utils::ErrorOr<std::remove_reference_t<
        decltype(std::declval<typename std::result_of<T(Args...)>::type>()
                     .Get())>> {
  try {
    return function(std::forward<Args>(params)...).Get();
  } catch (const CGAL::Failure_exception& e) {
    return CgalExceptionToError(e);
  }
}

}  // namespace cgal
}  // namespace hypo

#endif  // _HYPO_CGAL_ERRORS_H_
