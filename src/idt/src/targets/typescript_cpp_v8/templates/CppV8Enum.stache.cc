// {{!
// clang-format off
// }}

namespace {{namespace}} {

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
v8::MaybeLocal<{{{firstParam.type}}}> {{name}}::As{{__kind}}(
    v8::Local<v8::Context> context) {
  if (kind() != Kind_{{__kind}}) {
    return v8::MaybeLocal<{{{firstParam.type}}}>();
  }

  v8::Local<v8::String> key_name =
      v8::String::NewFromUtf8Literal(GetIsolate(), "{{firstParam.name}}");

  return Get(context, key_name).ToLocalChecked()
      .template As<{{{firstParam.type}}}>();
}
{{/unionMembers}}

}  // namespace {{namespace}}

{{immRefCntNamespace}}::{{name}} Value(
    v8::Isolate* isolate, v8::Local<{{namespace}}::{{name}}> x) {
  switch (x->kind()) {
    default:
      {{! // This lets us not only assert an impossibility, but fall through
          // to validly return something and avoid a compiler warning. }}
      assert(false);
{{#unionMembers}}
    case {{namespace}}::{{name}}::Kind_{{__kind}}:
      return Value(isolate, x->As{{__kind}}(isolate->GetCurrentContext())
                .ToLocalChecked());
{{/unionMembers}}
  }
}
