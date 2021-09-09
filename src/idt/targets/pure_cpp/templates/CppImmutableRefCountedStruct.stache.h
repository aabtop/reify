// {{!
// clang-format off
// }}

// {{comment}}
struct {{name}} {
{{#members}}
  // {{comment}}
  {{{type}}} {{name}};
{{/members}}
};

{{#enable_hashes}}

struct ObjectAndHash_{{name}} {
  ObjectAndHash_{{name}}({{name}}&& object, uint64_t hash) : object(object), hash(hash) {}
  operator {{name}}() const { return object; }

  {{name}} object;
  uint64_t hash;
};

inline void AddObjectToHash(blake3_hasher* hasher, const {{name}}& input) {
{{#members}}
  AddObjectToHash(hasher, input.{{name}});
{{/members}}
}

}  // {{namespace}}

namespace reify {

template <>
class Traits<{{namespace}}::{{name}}> {
 public:
  using Reference = std::shared_ptr<const {{namespace}}::ObjectAndHash_{{name}}>;

  static inline Reference New({{namespace}}::{{name}}&& x) {
    // Cache the hash of the referenced object.
    auto hash = HashObject(x);
    return std::make_shared<{{namespace}}::ObjectAndHash_{{name}}>(std::move(x), hash);
  }
};

inline const {{namespace}}::{{name}}& Deref(const std::shared_ptr<const {{namespace}}::ObjectAndHash_{{name}}>& x) { return x->object; }

}  // namespace reify

namespace {{namespace}} {

{{/enable_hashes}}
