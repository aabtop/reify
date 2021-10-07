#include "platform_specific/system_memory.h"

#include <Windows.h>

#include <cassert>

namespace reify {
namespace platform_specific {

int64_t TotalSystemMemoryCapacityInBytes() {
  ULONGLONG total_memory_in_kilobytes;
  BOOL result = GetPhysicallyInstalledSystemMemory(&total_memory_in_kilobytes);
  assert(result);
  return static_cast<int64_t>(total_memory_in_kilobytes * 1024);
}

}  // namespace platform_specific
}  // namespace reify
