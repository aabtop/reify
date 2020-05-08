// {{!
// clang-format off
// }}
{{immRefCntNamespace}}::{{name}} Value(
    v8::Isolate* isolate, v8::Local<{{name}}> x) {
  switch (x->kind()) {
{{#unionMembers}}
    case {{name}}::Kind_{{__kind}}:
      return Value(isolate, x->As{{__kind}}().ToLocalChecked());
{{/unionMembers}}
    default:
      assert(false);
  }
}
