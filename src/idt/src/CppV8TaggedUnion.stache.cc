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

{{immRefCntNamespace}}::{{name}} Value(
    v8::Isolate* isolate, v8::Local<{{name}}> x) {
  switch (x->kind()) {
    default:
      {{! // This lets us not only assert an impossibility, but fall through
          // to validly return something and avoid a compiler warning. }}
      assert(false);
{{#unionMembers}}
    case {{name}}::Kind_{{__kind}}:
      return Value(isolate, x.template As<{{{firstParam.type}}}>());
{{/unionMembers}}
  }
}
