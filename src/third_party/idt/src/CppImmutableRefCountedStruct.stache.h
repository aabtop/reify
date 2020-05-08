// {{!
// clang-format off
// }}
struct {{name}} {
{{#members}}
  {{{type}}} {{name}};
{{/members}}
};

static std::shared_ptr<{{name}}> New{{name}}({{name}}&& from) {
  return std::make_shared<{{name}}>(from);
}
