// {{!
// clang-format off
// }}

namespace {{namespace}} {

class {{name}} : public I32 {
 public:
  V8_INLINE static {{name}}* Cast(v8::Value* obj) {
#ifdef V8_ENABLE_CHECKS
    CheckCast(obj);
#endif
    return static_cast<{{name}}*>(obj);
  }

  {{immRefCntNamespace}}::{{name}} Value() {
    return static_cast<{{immRefCntNamespace}}::{{name}}>(I32::Value());
  }

 private:
  static void CheckCast(v8::Value* obj) {}
};

}  // {{namespace}}

template <>
struct Value<{{namespace}}::{{name}}> {
  static {{immRefCntNamespace}}::{{name}} Call(v8::Isolate* isolate, v8::Local<{{namespace}}::{{name}}> x) {
    return x->Value();
  }
};
template <>
struct FromImmRefCnt<{{immRefCntNamespace}}::{{name}}> {
  using type = {{namespace}}::{{name}};
};
template <>
struct TypeScriptTypeString<{{immRefCntNamespace}}::{{name}}> {
  static std::string value() { return "{{name}}"; }
};
