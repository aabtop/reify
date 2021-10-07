#ifndef _REIFY_PLATFORM_SPECIFIC_SYSTEM_MEMORY_H_
#define _REIFY_PLATFORM_SPECIFIC_SYSTEM_MEMORY_H_

#include <cstdint>

namespace reify {
namespace platform_specific {

// Returns the total available system memory in bytes.
int64_t TotalSystemMemoryCapacityInBytes();

}  // namespace platform_specific
}  // namespace reify

#endif  // _REIFY_PLATFORM_SPECIFIC_SYSTEM_MEMORY_H_
