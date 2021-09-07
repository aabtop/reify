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
inline void AddObjectToHash(blake3_hasher* hasher, const {{name}}& input) {
{{#members}}
  AddObjectToHash(hasher, input.{{name}});
{{/members}}
}
{{/enable_hashes}}
