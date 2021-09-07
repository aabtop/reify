// {{!
// clang-format off
// }}

namespace internal {
using {{name}}ParentType = std::variant<{{{comma_sep_types}}}>;
}  // namespace internal

// {{comment}}
class {{name}} : public internal::{{name}}ParentType {
  using ParentType = internal::{{name}}ParentType;
  using ParentType::ParentType;

 public:
  using AsVariant = ParentType;
};


{{#enable_hashes}}
inline void AddObjectToHash(blake3_hasher* hasher, const {{name}}& input) {
  size_t variant_index = input.index();
  blake3_hasher_update(hasher, 
    reinterpret_cast<const uint8_t*>(&variant_index), sizeof(variant_index));

  std::visit([hasher](const auto& arg) {
    AddObjectToHash(hasher, arg);    
  }, input);
}
{{/enable_hashes}}
