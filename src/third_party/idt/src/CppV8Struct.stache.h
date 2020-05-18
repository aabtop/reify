// {{!
// clang-format off
// }}
class {{name}} : public v8::Object {
 public:
  V8_INLINE static {{name}}* Cast(v8::Value* obj) {
#ifdef V8_ENABLE_CHECKS
    CheckCast(obj);
#endif
    return static_cast<{{name}}*>(obj);
  }

{{#members}}
  v8::Local<{{{type}}}> {{memberName}}();
{{/members}}

 private:
  static void CheckCast(v8::Value* obj) {}
};
{{immRefCntNamespace}}::{{name}} Value(
    v8::Isolate* isolate, v8::Local<{{name}}> x);
template <>
struct FromImmRefCnt<{{immRefCntNamespace}}::{{name}}> {
  using type = {{name}};
};
template <>
struct TypeMatchesTypeScriptString<{{immRefCntNamespace}}::{{name}}> {
  static bool Result(std::string_view ts) {
    return ts == "{{name}}";
  }
};
