// clang-format off
class {{name}} : public v8::Object {
 public:
  V8_INLINE static {{name}}* Cast(Value* obj) {
#ifdef V8_ENABLE_CHECKS
    CheckCast(obj);
#endif
    return static_cast<{{name}}*>(obj);
  }

  enum SubType {
{{#constructors}}
    {{__kind}},
{{/constructors}}
  };

  SubType sub_type() {
    std::string kind_value = internal::GetPropertyAsString(this, "__kind");
    if (false) {
  {{#constructors}}
    } else if (kind_value == "{{__kind}}") {
      return {{__kind}};
  {{/constructors}}
    } else {
      assert(false);
      return static_cast<SubType>(-1);
    }
  }

{{#constructors}}
  v8::MaybeLocal<{{{p0}}}> As{{__kind}}() {
    if (sub_type() != {{__kind}}) {
      return v8::MaybeLocal<{{{p0}}}>();
    }

    v8::Local<v8::String> key_name =
        v8::String::NewFromUtf8(GetIsolate(), "p0");

    return Get(key_name).template As<{{{p0}}}>();
  }
{{/constructors}}


 private:
  static void CheckCast(Value* obj) {}
};
