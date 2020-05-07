// {{!
// clang-format off
// }}


{{#constructors}}
struct {{cname}} {
{{#params}}
  {{{type}}} {{name}};
{{/params}}
};
{{/constructors}}

class {{name}} : public std::variant<{{{comma_sep_names}}}> {
  using VariantParentType = std::variant<{{{comma_sep_names}}}>;
  using VariantParentType::VariantParentType;
};
