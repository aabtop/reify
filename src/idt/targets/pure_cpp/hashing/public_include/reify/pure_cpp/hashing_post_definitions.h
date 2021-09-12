namespace reify {

template <typename T>
inline uint64_t HashObject(const T& input) {
  blake3_hasher hasher;
  blake3_hasher_init(&hasher);

  AddObjectToHash(&hasher, input);

  uint64_t result;
  blake3_hasher_finalize(&hasher, reinterpret_cast<uint8_t*>(&result),
                         sizeof(result));
  return result;
}

template <typename T>
inline uint64_t HashObject(const CachedHashReference<T>& input) {
  return input.hash();
}

template <typename T>
CachedHashReference<T>::CachedHashReference(BaseType&& x)
    : ptr_(std::make_shared<T>(std::move(x))), hash_(HashObject(*ptr_)) {}

}  // namespace reify
