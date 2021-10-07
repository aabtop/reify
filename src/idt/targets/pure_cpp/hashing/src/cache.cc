#include "reify/pure_cpp/cache.h"

namespace reify {
namespace pure_cpp {

void Cache::PurgeToCapacity() {
  while (approximate_cache_usage_ > capacity_) {
    // If we're over capacity, there should be a reason.
    assert(!ordering_.empty());

    const OrderingInfo& oldest_element_info = ordering_.back();
    oldest_element_info.cache->erase(oldest_element_info.hash);

    approximate_cache_usage_ -=
        oldest_element_info.estimated_memory_usage_in_bytes;
    ordering_.pop_back();
  }
}

}  // namespace pure_cpp
}  // namespace reify
