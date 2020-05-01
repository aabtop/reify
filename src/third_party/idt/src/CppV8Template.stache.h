// clang-format off
#include <tuple>

#include <v8.h>

namespace {{namespace}} {

namespace internal {
  std::string GetPropertyAsString(
      v8::Object* object, const char* key) {
    v8::Local<v8::String> key_name =
        v8::String::NewFromUtf8(object->GetIsolate(), key);

    v8::String::Utf8Value utf8_kind_value(
        object->GetIsolate(), object->Get(key_name).As<v8::String>());

    return std::string(*utf8_kind_value, utf8_kind_value.length());
  }
}  // namespace internal

template <typename T>
class List : public v8::Array {
 public:
  V8_INLINE static List<T>* Cast(Value* obj) {
#ifdef V8_ENABLE_CHECKS
    CheckCast(obj);
#endif
    return static_cast<List<T>*>(obj);
  }

  v8::MaybeLocal<T> Get(uint32_t index) {
    if (index >= Length()) {
      return v8::MaybeLocal<T>();
    }

    return v8::Array::Get(index).As<T>();
  }

 private:
  static void CheckCast(Value* obj) {}
};

namespace internal {
// From https://stackoverflow.com/questions/20162903/template-parameter-packs-access-nth-type-and-nth-element
template<int N, typename... Ts> using NthTypeOf =
        typename std::tuple_element<N, std::tuple<Ts...>>::type;
}  // namespace internal

template <typename... Types>
class Tuple : public v8::Array {
 public:
  V8_INLINE static Tuple* Cast(Value* obj) {
#ifdef V8_ENABLE_CHECKS
    CheckCast(obj);
#endif
    return static_cast<Tuple*>(obj);
  }

  template <int N>
  internal::NthTypeOf<N, Types...> Get() {
    return v8::Array::Get(N).As<internal::NthTypeOf<N, Types...>>();
  }

 private:
  static void CheckCast(Value* obj) {}
};

{{#declarationSequence}}
{{{.}}}
{{/declarationSequence}}

}  // {{namespace}}

