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
      return Value(isolate, x.template As<{{{firstParam.type}}}>());
{{/unionMembers}}
  }
}
