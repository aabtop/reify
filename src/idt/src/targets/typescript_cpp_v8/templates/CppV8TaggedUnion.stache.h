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

 private:
  static void CheckCast(v8::Value* obj) {}
};

}  // namespace {{namespace}}

{{immRefCntNamespace}}::{{name}} Value(
    v8::Isolate* isolate, v8::Local<{{namespace}}::{{name}}> x);
template <>
struct FromImmRefCnt<{{immRefCntNamespace}}::{{name}}> {
  using type = {{namespace}}::{{name}};
};
template <>
struct TypeMatchesTypeScriptString<{{immRefCntNamespace}}::{{name}}> {
  static bool Result(std::string_view ts) {
    return ts == "{{name}}"
{{#unionMembers}}
           || ts == "{{__kind}}"
{{/unionMembers}}
    ;
  }
};