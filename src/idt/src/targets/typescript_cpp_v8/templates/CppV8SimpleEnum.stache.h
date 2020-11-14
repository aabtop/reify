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

inline {{immRefCntNamespace}}::{{name}} Value(
    v8::Isolate* isolate, v8::Local<{{namespace}}::{{name}}> x) {
  return x->Value();
}
template <>
struct FromImmRefCnt<{{immRefCntNamespace}}::{{name}}> {
  using type = {{namespace}}::{{name}};
};
template <>
struct TypeMatchesTypeScriptString<{{immRefCntNamespace}}::{{name}}> {
  static bool Result(std::string_view ts) {
    return ts == "{{name}}";
  }
};
