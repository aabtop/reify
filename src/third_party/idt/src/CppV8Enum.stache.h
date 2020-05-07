// clang-format off
class {{name}} : public v8::Object {
 public:
  V8_INLINE static {{name}}* Cast(Value* obj) {
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

  Kind kind() {
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

{{#unionMembers}}
  v8::MaybeLocal<{{{firstParam.type}}}> As{{__kind}}() {
    if (kind() != Kind_{{__kind}}) {
      return v8::MaybeLocal<{{{firstParam.type}}}>();
    }

    v8::Local<v8::String> key_name =
        v8::String::NewFromUtf8(GetIsolate(), "{{firstParam.name}}");

    return Get(key_name).template As<{{{firstParam.type}}}>();
  }
{{/unionMembers}}


 private:
  static void CheckCast(Value* obj) {}
};
