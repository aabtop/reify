#ifndef _REIFY_COMMON_TYPES_H_
#define _REIFY_COMMON_TYPES_H_

#include <v8.h>

#include <cassert>
#include <tuple>
#include <vector>

namespace reify_v8 {

template <typename T>
struct FromImmRefCnt {};

// Parses a TypeScript string and returns true if it matches the passed in
// C++ string.
template <typename T>
struct TypeMatchesTypeScriptString {};

void InstallInterfaceToGlobalObject(
    v8::Isolate* isolate, v8::Local<v8::ObjectTemplate> global_template);

namespace internal {
inline std::string GetPropertyAsString(v8::Object* object, const char* key) {
  v8::Local<v8::String> key_name =
      v8::String::NewFromUtf8(object->GetIsolate(), key).ToLocalChecked();

  v8::String::Utf8Value utf8_kind_value(
      object->GetIsolate(), object->Get(object->CreationContext(), key_name)
                                .ToLocalChecked()
                                .template As<v8::String>());

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
template <>
struct FromImmRefCnt<std::string> {
  using type = v8::String;
};
template <>
struct TypeMatchesTypeScriptString<std::string> {
  static bool Result(std::string_view ts) { return ts == "string"; }
};

class I32 : public v8::Number {
 public:
  V8_INLINE static I32* Cast(v8::Value* obj) {
#ifdef V8_ENABLE_CHECKS
    CheckCast(obj);
#endif
    return static_cast<I32*>(obj);
  }

  int32_t Value() const { return static_cast<int32_t>(v8::Number::Value()); }

 private:
  static void CheckCast(v8::Value* obj) {}
};
V8_INLINE int32_t Value(v8::Isolate* isolate, v8::Local<I32> x) {
  return x->Value();
}
template <>
struct FromImmRefCnt<int32_t> {
  using type = I32;
};
template <>
struct TypeMatchesTypeScriptString<int32_t> {
  static bool Result(std::string_view ts) { return ts == "number"; }
};

class F32 : public v8::Number {
 public:
  V8_INLINE static F32* Cast(v8::Value* obj) {
#ifdef V8_ENABLE_CHECKS
    CheckCast(obj);
#endif
    return static_cast<F32*>(obj);
  }

  float Value() const { return static_cast<float>(v8::Number::Value()); }

 private:
  static void CheckCast(v8::Value* obj) {}
};
V8_INLINE float Value(v8::Isolate* isolate, v8::Local<F32> x) {
  return x->Value();
}
template <>
struct FromImmRefCnt<float> {
  using type = F32;
};
template <>
struct TypeMatchesTypeScriptString<float> {
  static bool Result(std::string_view ts) { return ts == "number"; }
};

V8_INLINE bool Value(v8::Isolate* isolate, v8::Local<v8::Boolean> x) {
  return x->Value();
}
template <>
struct FromImmRefCnt<bool> {
  using type = v8::Boolean;
};
template <>
struct TypeMatchesTypeScriptString<bool> {
  static bool Result(std::string_view ts) { return ts == "boolean"; }
};

template <typename T>
class List : public v8::Array {
 public:
  V8_INLINE static List<T>* Cast(v8::Value* obj) {
#ifdef V8_ENABLE_CHECKS
    CheckCast(obj);
#endif
    return static_cast<List<T>*>(obj);
  }

  v8::MaybeLocal<T> Get(v8::Local<v8::Context> context, uint32_t index) {
    if (index >= Length()) {
      return v8::MaybeLocal<T>();
    }

    return v8::Array::Get(context, index).ToLocalChecked().template As<T>();
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
    ret.push_back(Value(
        isolate, x->Get(isolate->GetCurrentContext(), i).ToLocalChecked()));
  }
  return ret;
}
template <typename T>
struct FromImmRefCnt<std::vector<T>> {
  using type = List<typename FromImmRefCnt<T>::type>;
};
template <typename T>
struct TypeMatchesTypeScriptString<std::vector<T>> {
  static bool Result(std::string_view ts) {
    assert(false);  // Not implemented.
    return false;
  }
};

template <typename T, int S>
class FixedSizeArray : public v8::Array {
 public:
  V8_INLINE static FixedSizeArray<T, S>* Cast(v8::Value* obj) {
#ifdef V8_ENABLE_CHECKS
    CheckCast(obj);
#endif
    return static_cast<FixedSizeArray<T, S>*>(obj);
  }

  v8::Local<T> Get(v8::Local<v8::Context> context, uint32_t index) {
    assert(index < S && index >= 0);
    return v8::Array::Get(context, index).ToLocalChecked().template As<T>();
  }

 private:
  static void CheckCast(v8::Value* obj) {}
};

template <typename T, int S>
auto Value(v8::Isolate* isolate, v8::Local<FixedSizeArray<T, S>> x);

namespace internal {
template <typename T, int S, std::size_t... Indices>
auto Value(v8::Isolate* isolate, v8::Local<FixedSizeArray<T, S>> x,
           indices<Indices...>) {
  return std::array<
      decltype(reify_v8::Value(isolate, std::declval<v8::Local<T>>())), S>{
      reify_v8::Value(isolate,
                      x->Get(isolate->GetCurrentContext(), Indices))...};
}
}  // namespace internal

template <typename T, int S>
auto Value(v8::Isolate* isolate, v8::Local<FixedSizeArray<T, S>> x) {
  return internal::Value(isolate, x, internal::build_indices<S>{});
}
template <typename T, int S>
struct FromImmRefCnt<std::array<T, S>> {
  using type = FixedSizeArray<typename FromImmRefCnt<T>::type, S>;
};
template <typename T, int S>
struct TypeMatchesTypeScriptString<std::array<T, S>> {
  static bool Result(std::string_view ts) {
    assert(false);  // Not implemented.
    return false;
  }
};

namespace internal {
// From
// https://stackoverflow.com/questions/20162903/template-parameter-packs-access-nth-type-and-nth-element
template <int N, typename... Ts>
using NthTypeOf = typename std::tuple_element<N, std::tuple<Ts...>>::type;
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
  v8::Local<internal::NthTypeOf<N, Types...>> Get(
      v8::Local<v8::Context> context) {
    return v8::Array::Get(context, N)
        .ToLocalChecked()
        .template As<internal::NthTypeOf<N, Types...>>();
  }

 private:
  static void CheckCast(v8::Value* obj) {}
};

namespace internal {
template <typename... Ts, std::size_t... Indices>
auto Value(v8::Isolate* isolate, v8::Local<Tuple<Ts...>>& x,
           indices<Indices...>) {
  return std::make_tuple(
      reify_v8::Value(isolate, x->template Get<Indices>())...);
}
}  // namespace internal

template <typename... Ts>
auto Value(v8::Isolate* isolate, v8::Local<Tuple<Ts...>> x) {
  return internal::Value(isolate, x, internal::build_indices<sizeof...(Ts)>{});
}
template <typename... Ts>
struct FromImmRefCnt<std::tuple<Ts...>> {
  using type = Tuple<typename FromImmRefCnt<Ts>::type...>;
};
template <typename... Ts>
struct TypeMatchesTypeScriptString<std::tuple<Ts...>> {
  static bool Result(std::string_view ts) {
    assert(false);  // Not implemented.
    return false;
  }
};

}  // namespace reify_v8

#endif  // _REIFY_COMMON_TYPES_H_
