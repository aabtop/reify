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
