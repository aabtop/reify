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
  v8::Local<{{{type}}}> {{name}}() {
    v8::Local<v8::String> key_name =
        v8::String::NewFromUtf8(GetIsolate(), "{{name}}");

    return Get(key_name).template As<{{{type}}}>();
  }
{{/members}}

 private:
  static void CheckCast(v8::Value* obj) {}
};
{{immRefCntNamespace}}::{{name}} Value(
    v8::Isolate* isolate, v8::Local<{{name}}> x);
