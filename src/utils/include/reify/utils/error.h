#ifndef _SRC_UTILS_INCLUDE_REIFY_UTILS_ERROR_OR_H_
#define _SRC_UTILS_INCLUDE_REIFY_UTILS_ERROR_OR_H_

#include <optional>
#include <string>
#include <variant>

namespace reify {
namespace utils {

struct Error {
  std::string msg;
};

template <typename T>
using ErrorOr = std::variant<Error, T>;
using MaybeError = std::optional<Error>;

#define REIFY_UTILS_ASSIGN_OR_RETURN(lhs, rhs)        \
  auto error_or_##lhs = rhs;                          \
  if (auto error = std::get_if<0>(&error_or_##lhs)) { \
    return *error;                                    \
  }                                                   \
  auto& lhs = std::get<1>(error_or_##lhs)

}  // namespace utils
}  // namespace reify

#endif  // _SRC_UTILS_INCLUDE_REIFY_UTILS_ERROR_OR_H_
