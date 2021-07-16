// {{!
// clang-format off
// }}

namespace {{namespace}} {

class {{name}} : public v8::Object {
 public:
  V8_INLINE static {{name}}* Cast(v8::Value* obj) {
#ifdef V8_ENABLE_CHECKS
    CheckCast(obj);
#endif
    return static_cast<{{name}}*>(obj);
  }

  enum Kind {
{{#unionMembers}}
    Kind_{{__kind}},
{{/unionMembers}}
  };

  Kind kind();

{{#unionMembers}}
  v8::MaybeLocal<{{{firstParam.type}}}> As{{__kind}}(v8::Local<v8::Context> context);
{{/unionMembers}}


 private:
  static void CheckCast(v8::Value* obj) {}
};

}  // {{namespace}}

template <>
struct Value<{{namespace}}::{{name}}> {
  static {{immRefCntNamespace}}::{{name}} Call(v8::Isolate* isolate, v8::Local<{{namespace}}::{{name}}> x);
};
template <>
struct FromImmRefCnt<{{immRefCntNamespace}}::{{name}}> {
  using type = {{namespace}}::{{name}};
};
template <>
struct TypeScriptTypeString<{{immRefCntNamespace}}::{{name}}> {
  static std::string value() { return "{{name}}"; }
};
