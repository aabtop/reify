#include "platform_specific/system_memory.h"

#include <sys/sysinfo.h>
#include <unistd.h>

#include <cassert>
#include <fstream>

namespace reify {
namespace platform_specific {

int64_t TotalSystemMemoryCapacityInBytes() {
  struct sysinfo si;
  int result = sysinfo(&si);
  assert(result == 0);
  return static_cast<int64_t>(si.totalram);
}

int64_t MemoryResidentForCurrentProcess() {
  int64_t size = 0, resident = 0, share = 0;
  std::ifstream buffer("/proc/self/statm");
  buffer >> size >> resident >> share;
  buffer.close();

  long page_size_kb = sysconf(_SC_PAGE_SIZE) /
                      1024;  // in case x86-64 is configured to use 2MB pages
  int64_t rss = resident * page_size_kb;
  return rss;
}

}  // namespace platform_specific
}  // namespace reify
