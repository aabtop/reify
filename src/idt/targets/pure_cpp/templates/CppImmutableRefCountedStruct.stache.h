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

}  // {{namespace}}

namespace reify {

inline void AddObjectToHash(blake3_hasher* hasher, const {{namespace}}::{{name}}& input) {
{{#members}}
  AddObjectToHash(hasher, input.{{name}});
{{/members}}
}

inline CachedHashReference<{{namespace}}::{{name}}> New({{namespace}}::{{name}}&& x) {
  return CachedHashReference<{{namespace}}::{{name}}>(std::move(x));
}

}  // namespace reify

namespace {{namespace}} {

{{/enable_hashes}}
