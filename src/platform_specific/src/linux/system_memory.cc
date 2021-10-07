#include "platform_specific/system_memory.h"

#include <sys/sysinfo.h>

#include <cassert>

namespace reify {
namespace platform_specific {

int64_t TotalSystemMemoryCapacityInBytes() {
  struct sysinfo si;
  int result = sysinfo(&si);
  assert(result == 0);
  return static_cast<int64_t>(si.totalram);
}

}  // namespace platform_specific
}  // namespace reify
