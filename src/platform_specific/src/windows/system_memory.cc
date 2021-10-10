#include "platform_specific/system_memory.h"

// clang-format off
#include <windows.h>
// clang-format on

#include <psapi.h>

#include <cassert>

namespace reify {
namespace platform_specific {

int64_t TotalSystemMemoryCapacityInBytes() {
  ULONGLONG total_memory_in_kilobytes;
  BOOL result = GetPhysicallyInstalledSystemMemory(&total_memory_in_kilobytes);
  assert(result);
  return static_cast<int64_t>(total_memory_in_kilobytes * 1024);
}

int64_t MemoryResidentForCurrentProcess() {
  PROCESS_MEMORY_COUNTERS counters;
  BOOL result =
      GetProcessMemoryInfo(GetCurrentProcess(), &counters, sizeof(counters));
  assert(result);
  return static_cast<int64_t>(counters.WorkingSetSize);
}

}  // namespace platform_specific
}  // namespace reify
