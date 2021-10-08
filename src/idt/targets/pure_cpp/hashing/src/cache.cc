#include "reify/pure_cpp/cache.h"

namespace reify {
namespace pure_cpp {

void Cache::PurgeToCapacity(std::unique_lock<std::mutex> lock_to_release) {
  assert(lock_to_release.owns_lock());

  // We don't delete items right away as we purge, we store them and delete
  // them after releasing the lock in case destruction takes a while. Note
  // that because we're doing this, it's possible that we allocate a lot of
  // memory in the cache before the to-destroy items have been destroyed, and
  // use up lots of memory. Hopefully that doesn't happen.
  std::vector<std::any> items_to_destroy;

  {
    // Release the lock before returning. The items_to_destroy destructor will
    // then be responsible for actually destroying the elements.
    std::unique_lock<std::mutex> lock(std::move(lock_to_release));

    while (approximate_cache_usage_ > capacity_) {
      // If we're over capacity, there should be a reason.
      assert(!ordering_.empty());

      const OrderingInfo& oldest_element_info = ordering_.back();

      auto item_iter =
          oldest_element_info.cache->find(oldest_element_info.hash);
      std::any* item = std::get_if<std::any>(&(item_iter->second));
      assert(item);  // The alternative is that this is an item that's still
                     // being constructed, but that should never happen because
                     // we don't insert the item into the order queue until it
                     // has been fully constructed.
      items_to_destroy.push_back(*item);
      oldest_element_info.cache->erase(item_iter);

      approximate_cache_usage_ -=
          oldest_element_info.estimated_memory_usage_in_bytes;
      ordering_.pop_back();
    }
  }
}

}  // namespace pure_cpp
}  // namespace reify
