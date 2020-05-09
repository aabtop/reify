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

  enum Kind {
{{#unionMembers}}
    Kind_{{__kind}},
{{/unionMembers}}
  };

  Kind kind();

{{#unionMembers}}
  v8::MaybeLocal<{{{firstParam.type}}}> As{{__kind}}();
{{/unionMembers}}


 private:
  static void CheckCast(v8::Value* obj) {}
};
{{immRefCntNamespace}}::{{name}} Value(
    v8::Isolate* isolate, v8::Local<{{name}}> x);
