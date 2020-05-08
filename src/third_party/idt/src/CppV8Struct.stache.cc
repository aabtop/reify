// {{!
// clang-format off
// }}
{{immRefCntNamespace}}::{{name}} Value(
    v8::Isolate* isolate, v8::Local<{{name}}> x) {
  return {{immRefCntNamespace}}::{{name}}({
  {{#members}}
    .{{name}} = Value(isolate, x->{{name}}()),
  {{/members}}
  });
}
