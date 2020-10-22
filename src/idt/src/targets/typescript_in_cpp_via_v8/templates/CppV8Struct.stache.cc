// {{!
// clang-format off
// }}
{{immRefCntNamespace}}::{{name}} Value(
    v8::Isolate* isolate, v8::Local<{{name}}> x) {
  return {{immRefCntNamespace}}::{{name}}({
  {{#members}}
    /*.{{memberName}} = */Value(isolate, x->{{memberName}}(isolate->GetCurrentContext())),
  {{/members}}
  });
}

{{#members}}
v8::Local<{{{type}}}> {{name}}::{{memberName}}(v8::Local<v8::Context> context) {
  v8::Local<v8::String> key_name =
      v8::String::NewFromUtf8Literal(GetIsolate(), "{{memberName}}");

  return Get(context, key_name).ToLocalChecked().template As<{{{type}}}>();
}
{{/members}}
