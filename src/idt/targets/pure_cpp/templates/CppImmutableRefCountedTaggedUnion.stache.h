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

}  // {{namespace}}

namespace reify {

inline CachedHashReference<{{namespace}}::{{name}}> New({{namespace}}::{{name}}&& x) {
  return CachedHashReference<{{namespace}}::{{name}}>(std::move(x));
}

}  // namespace reify

namespace {{namespace}} {

{{/enable_hashes}}
