// clang-format off
class {{name}} : public std::variant<{{{comma_sep_types}}}> {
  using VariantParentType = std::variant<{{{comma_sep_types}}}>;
  using VariantParentType::VariantParentType;
};

{{#constructors}}
{{name}} {{cname}}(const {{{p0}}}& x) { return {{name}}(x); }
{{/constructors}}
