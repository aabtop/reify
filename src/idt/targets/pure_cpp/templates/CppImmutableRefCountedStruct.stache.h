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

inline std::shared_ptr<const {{name}}> New{{name}}({{name}}&& from) {
  return std::make_shared<{{name}}>(std::move(from));
}
