// {{!
// clang-format off
// }}
#ifndef _{{namespace}}_CPP_V8_IST_GENERATED_H_
#define _{{namespace}}_CPP_V8_IST_GENERATED_H_

#include <cassert>
#include <tuple>
#include <vector>

#include <v8.h>

#include "{{immutableRefCountedHeaderFile}}"

namespace {{namespace}} {

void InstallInterfaceToGlobalObject(
    v8::Isolate* isolate, v8::Local<v8::ObjectTemplate> global_template);

namespace internal {
inline std::string GetPropertyAsString(
    v8::Object* object, const char* key) {
  v8::Local<v8::String> key_name =
      v8::String::NewFromUtf8(object->GetIsolate(), key);

  v8::String::Utf8Value utf8_kind_value(
      object->GetIsolate(), object->Get(key_name).template As<v8::String>());

  return std::string(*utf8_kind_value, utf8_kind_value.length());
}

template <std::size_t... Is>
struct indices {};

template <std::size_t N, std::size_t... Is>
struct build_indices : build_indices<N - 1, N - 1, Is...> {};

template <std::size_t... Is>
struct build_indices<0, Is...> : indices<Is...> {};

}  // namespace internal

V8_INLINE std::string Value(v8::Isolate* isolate, v8::Local<v8::String> x) {
  v8::String::Utf8Value utf8_value(isolate, x);
  return std::string(*utf8_value, utf8_value.length());
}

class I32 : public v8::Number {
 public:
  V8_INLINE static I32* Cast(v8::Value* obj) {
#ifdef V8_ENABLE_CHECKS
    CheckCast(obj);
#endif
    return static_cast<I32*>(obj);
  }

  int32_t Value() const {
    return static_cast<int32_t>(v8::Number::Value());
  }

 private:
  static void CheckCast(v8::Value* obj) {}
};
V8_INLINE int32_t Value(v8::Isolate* isolate, v8::Local<I32> x) {
  return x->Value();
}

class F32 : public v8::Number {
 public:
  V8_INLINE static F32* Cast(v8::Value* obj) {
#ifdef V8_ENABLE_CHECKS
    CheckCast(obj);
#endif
    return static_cast<F32*>(obj);
  }

  float Value() const {
    return static_cast<float>(v8::Number::Value());
  }

 private:
  static void CheckCast(v8::Value* obj) {}
};
V8_INLINE float Value(v8::Isolate* isolate, v8::Local<F32> x) {
  return x->Value();
}

template <typename T>
class List : public v8::Array {
 public:
  V8_INLINE static List<T>* Cast(v8::Value* obj) {
#ifdef V8_ENABLE_CHECKS
    CheckCast(obj);
#endif
    return static_cast<List<T>*>(obj);
  }

  v8::MaybeLocal<T> Get(uint32_t index) {
    if (index >= Length()) {
      return v8::MaybeLocal<T>();
    }

    return v8::Array::Get(index).template As<T>();
  }

 private:
  static void CheckCast(v8::Value* obj) {}
};
template <typename T>
auto Value(v8::Isolate* isolate, v8::Local<List<T>> x) {
  int length = x->Length();
  std::vector<decltype(Value(isolate, std::declval<v8::Local<T>>()))> ret;
  ret.reserve(length);
  for (int i = 0; i < length; ++i) {
    ret.push_back(Value(isolate, x->Get(i).ToLocalChecked()));
  }
  return ret;
}

template <typename T, int S>
class FixedSizeArray : public v8::Array {
 public:
  V8_INLINE static FixedSizeArray<T, S>* Cast(v8::Value* obj) {
#ifdef V8_ENABLE_CHECKS
    CheckCast(obj);
#endif
    return static_cast<FixedSizeArray<T, S>*>(obj);
  }

  v8::Local<T> Get(uint32_t index) {
    assert(index < S && index >= 0);
    return v8::Array::Get(index).template As<T>();
  }

 private:
  static void CheckCast(v8::Value* obj) {}
};
namespace internal {
template <typename T, int S, std::size_t... Indices>
auto Value(v8::Isolate* isolate, v8::Local<FixedSizeArray<T, S>> x,
           indices<Indices...>) {
  return std::array<decltype(Value(isolate, std::declval<v8::Local<T>>())), S>{
      {{namespace}}::Value(isolate, x->template Get(Indices))...};
}

}  // namespace internal

template <typename T, int S>
auto Value(v8::Isolate* isolate, v8::Local<FixedSizeArray<T, S>> x) {
  return internal::Value(isolate, x, internal::build_indices<S>{});
}

namespace internal {
// From https://stackoverflow.com/questions/20162903/template-parameter-packs-access-nth-type-and-nth-element
template<int N, typename... Ts> using NthTypeOf =
        typename std::tuple_element<N, std::tuple<Ts...>>::type;
}  // namespace internal

template <typename... Types>
class Tuple : public v8::Array {
 public:
  V8_INLINE static Tuple* Cast(v8::Value* obj) {
#ifdef V8_ENABLE_CHECKS
    CheckCast(obj);
#endif
    return static_cast<Tuple*>(obj);
  }

  template <int N>
  v8::Local<internal::NthTypeOf<N, Types...>> Get() {
    return v8::Array::Get(N).template As<internal::NthTypeOf<N, Types...>>();
  }

 private:
  static void CheckCast(v8::Value* obj) {}
};

namespace internal {
template <typename... Ts, std::size_t... Indices>
auto Value(v8::Isolate* isolate, v8::Local<Tuple<Ts...>>& x,
           indices<Indices...>) {
  return std::make_tuple(
      {{namespace}}::Value(isolate, x->template Get<Indices>())...);
}
}  // namespace internal

template <typename... Ts>
auto Value(v8::Isolate* isolate, v8::Local<Tuple<Ts...>> x) {
  return internal::Value(isolate, x, internal::build_indices<sizeof...(Ts)>{});
}

template <typename T>
class Ref : public T {
 public:
  V8_INLINE static Ref<T>* Cast(v8::Value* obj) {
    return static_cast<Ref<T>*>(T::Cast(obj));
  }
};
template <typename T>
auto Value(v8::Isolate* isolate, v8::Local<Ref<T>> x) {
  using ValueType = decltype(Value(isolate, std::declval<v8::Local<T>>()));
  return std::make_shared<ValueType>(Value(isolate, v8::Local<T>::Cast(x)));
}

{{#declarationSequence}}
{{{.}}}
{{/declarationSequence}}

}  // {{namespace}}

#endif  // _{{namespace}}_CPP_V8_IST_GENERATED_H_