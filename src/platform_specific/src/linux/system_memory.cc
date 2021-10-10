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
  int tSize = 0, resident = 0, share = 0;
  ifstream buffer("/proc/self/statm");
  buffer >> tSize >> resident >> share;
  buffer.close();

  long page_size_kb = sysconf(_SC_PAGE_SIZE) /
                      1024;  // in case x86-64 is configured to use 2MB pages
  double rss = resident * page_size_kb;
  cout << "RSS - " << rss << " kB\n";

  double shared_mem = share * page_size_kb;
  cout << "Shared Memory - " << shared_mem << " kB\n";

  cout << "Private Memory - " << rss - shared_mem << "kB\n";
  return 0;
}

}  // namespace platform_specific
}  // namespace reify
