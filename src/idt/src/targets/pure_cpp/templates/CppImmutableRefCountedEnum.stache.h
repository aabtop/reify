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

{{{tagged_union_def}}}