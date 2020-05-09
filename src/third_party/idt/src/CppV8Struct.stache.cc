// {{!
// clang-format off
// }}
{{immRefCntNamespace}}::{{name}} Value(
    v8::Isolate* isolate, v8::Local<{{name}}> x) {
  return {{immRefCntNamespace}}::{{name}}({
  {{#members}}
    .{{memberName}} = Value(isolate, x->{{memberName}}()),
  {{/members}}
  });
}

{{#members}}
v8::Local<{{{type}}}> {{name}}::{{memberName}}() {
  v8::Local<v8::String> key_name =
      v8::String::NewFromUtf8(GetIsolate(), "{{memberName}}");

  return Get(key_name).template As<{{{type}}}>();
}
{{/members}}
