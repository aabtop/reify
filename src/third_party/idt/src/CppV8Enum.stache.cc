// {{!
// clang-format off
// }}

auto {{name}}::kind() -> Kind {
  std::string kind_value = internal::GetPropertyAsString(this, "__kind");
  if (false) {
{{#unionMembers}}
  } else if (kind_value == "{{__kind}}") {
    return Kind_{{__kind}};
{{/unionMembers}}
  } else {
    assert(false);
    return static_cast<Kind>(-1);
  }
}

{{#unionMembers}}
v8::MaybeLocal<{{{firstParam.type}}}> {{name}}::As{{__kind}}() {
  if (kind() != Kind_{{__kind}}) {
    return v8::MaybeLocal<{{{firstParam.type}}}>();
  }

  v8::Local<v8::String> key_name =
      v8::String::NewFromUtf8(GetIsolate(), "{{firstParam.name}}");

  return Get(key_name).template As<{{{firstParam.type}}}>();
}
{{/unionMembers}}

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
