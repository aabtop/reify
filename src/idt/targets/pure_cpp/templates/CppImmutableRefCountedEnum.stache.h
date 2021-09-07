// {{!
// clang-format off
// }}
{{#constructors}}
struct {{cname}} {
{{#params}}
  {{{type}}} {{name}};
{{/params}}
};

{{#enable_hashes}}
inline void AddObjectToHash(blake3_hasher* hasher, const {{cname}}& input) {
  for (const auto& i : input) {
    AddObjectToHash(hasher, i);
  }
}
{{/enable_hashes}}

{{/constructors}}

{{{tagged_union_def}}}
