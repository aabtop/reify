#ifndef _REIFY_IDT_TARGETS_TYPESCRIPT_CPP_V8_COMMON_TYPES_H_
#define _REIFY_IDT_TARGETS_TYPESCRIPT_CPP_V8_COMMON_TYPES_H_

#include <v8.h>

#include <cassert>
#include <tuple>
#include <vector>

#include "reify/pure_cpp/hashing.h"

namespace reify_v8 {

template <typename T>
struct Value {};

template <typename T>
struct FromImmRefCnt {};

template <typename T>
struct TypeScriptTypeString {};

// Parses a TypeScript string and returns true if it matches the passed in
// C++ string.
template <typename T>
struct TypeMatchesTypeScriptString {
  static bool Result(std::string_view ts) {
    return ts == TypeScriptTypeString<T>::value();
  }
};

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

template <>
struct Value<v8::String> {
  static std::string Call(v8::Isolate* isolate, v8::Local<v8::String> x) {
    v8::String::Utf8Value utf8_value(isolate, x);
    return std::string(*utf8_value, utf8_value.length());
  }
};

template <>
struct FromImmRefCnt<std::string> {
  using type = v8::String;
};
template <>
struct TypeScriptTypeString<std::string> {
  static std::string value() { return "string"; }
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

template <>
struct Value<I32> {
  static int32_t Call(v8::Isolate* isolate, v8::Local<I32> x) {
    return x->Value();
  }
};
template <>
struct FromImmRefCnt<int32_t> {
  using type = I32;
};
template <>
struct TypeScriptTypeString<int32_t> {
  static std::string value() { return "number"; }
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
template <>
struct Value<F32> {
  static float Call(v8::Isolate* isolate, v8::Local<F32> x) {
    return x->Value();
  }
};
template <>
struct FromImmRefCnt<float> {
  using type = F32;
};
template <>
struct TypeScriptTypeString<float> {
  static std::string value() { return "number"; }
};

template <>
struct Value<v8::Boolean> {
  static bool Call(v8::Isolate* isolate, v8::Local<v8::Boolean> x) {
    return x->Value();
  }
};
template <>
struct FromImmRefCnt<bool> {
  using type = v8::Boolean;
};
template <>
struct TypeScriptTypeString<bool> {
  static std::string value() { return "boolean"; }
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
struct Value<List<T>> {
  static auto Call(v8::Isolate* isolate, v8::Local<List<T>> x) {
    int length = x->Length();
    std::vector<decltype(Value<T>::Call(isolate, std::declval<v8::Local<T>>()))>
        ret;
    ret.reserve(length);
    for (int i = 0; i < length; ++i) {
      ret.push_back(Value<T>::Call(
          isolate, x->Get(isolate->GetCurrentContext(), i).ToLocalChecked()));
    }
    return ret;
  }
};
template <typename T>
struct FromImmRefCnt<std::vector<T>> {
  using type = List<typename FromImmRefCnt<T>::type>;
};
template <typename T>
struct TypeScriptTypeString<std::vector<T>> {
  static std::string value() { return TypeScriptTypeString<T>::value() + "[]"; }
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
struct Value<FixedSizeArray<T, S>> {
  template <std::size_t... Indices>
  static auto CallInternal(v8::Isolate* isolate,
                           v8::Local<FixedSizeArray<T, S>> x,
                           internal::indices<Indices...>) {
    return std::array<
        decltype(Value<T>::Call(isolate, std::declval<v8::Local<T>>())), S>{
        Value<T>::Call(isolate,
                       x->Get(isolate->GetCurrentContext(), Indices))...};
  }
  static auto Call(v8::Isolate* isolate, v8::Local<FixedSizeArray<T, S>> x) {
    return CallInternal(isolate, x, internal::build_indices<S>{});
  }
};

template <typename T, int S>
struct FromImmRefCnt<std::array<T, S>> {
  using type = FixedSizeArray<typename FromImmRefCnt<T>::type, S>;
};
template <typename T, int S>
struct TypeScriptTypeString<std::array<T, S>> {
  // Not implemented.
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

template <typename... Ts>
struct Value<Tuple<Ts...>> {
  template <std::size_t... Indices>
  static auto CallInternal(v8::Isolate* isolate, v8::Local<Tuple<Ts...>>& x,
                           internal::indices<Indices...>) {
    return std::make_tuple(Value<decltype(x->template Get<Indices>())>::Call(
        isolate, x->template Get<Indices>())...);
  }
  static auto Call(v8::Isolate* isolate, v8::Local<Tuple<Ts...>> x) {
    return CallInternal(isolate, x, internal::build_indices<sizeof...(Ts)>{});
  }
};

template <typename... Ts>
struct FromImmRefCnt<std::tuple<Ts...>> {
  using type = Tuple<typename FromImmRefCnt<Ts>::type...>;
};
template <typename... Ts>
struct TypeScriptTypeString<std::tuple<Ts...>> {
  // Not implemented.
};

template <typename T>
class Ref : public T {
 public:
  using DerefValueType = const decltype(reify_v8::Value<T>::Call(
      std::declval<v8::Isolate*>(), std::declval<v8::Local<T>>()));
  using RefValueType =
      decltype(::reify::New(std::declval<std::decay_t<DerefValueType>>()));

  V8_INLINE static Ref<T>* Cast(v8::Value* obj) {
    return static_cast<Ref<T>*>(T::Cast(obj));
  }
};

template <typename T>
typename Ref<T>::RefValueType ConstructRefValue(v8::Isolate* isolate,
                                                v8::Local<Ref<T>> x) {
  return ::reify::New(Value<T>::Call(isolate, v8::Local<T>::Cast(x)));
}

template <typename T>
struct WeakCallbackData {
  WeakCallbackData(v8::Isolate* isolate, v8::Local<Ref<T>> x)
      : persistent(isolate, x), ptr_data(ConstructRefValue(isolate, x)) {}

  // Unfortunately, we can't setup a floating callback without also keeping
  // a v8::Persistent object alive, so we bundle it here with the data.
  v8::Persistent<Ref<T>> persistent;
  typename Ref<T>::RefValueType ptr_data;
};

template <typename T>
struct Value<Ref<T>> {
  static auto Call(v8::Isolate* isolate, v8::Local<Ref<T>> x) {
    if constexpr (std::is_base_of<v8::Object, T>::value) {
      if (x->InternalFieldCount() > 0) {
        // If we have an internal field (i.e. we were created from an object
        // template), cache the returned shared_ptr value into the internal
        // field and then return that.
        auto internal_field = static_cast<WeakCallbackData<T>*>(
            x->GetAlignedPointerFromInternalField(0));
        if (!internal_field) {
          internal_field = new WeakCallbackData<T>(isolate, x);

          x->SetAlignedPointerInInternalField(0, internal_field);

          // We package up a pointer to the Persistent<Ref<T>> object itself
          // into the callback so that we can reset it after it has been cleaned
          // up, as the V8 API requires.
          internal_field->persistent.SetWeak(
              internal_field,
              [](const v8::WeakCallbackInfo<WeakCallbackData<T>>& data) {
                WeakCallbackData<T>* obj = data.GetParameter();
                obj->persistent.Reset();
                delete obj;
              },
              v8::WeakCallbackType::kParameter);
        }
        return internal_field->ptr_data;
      } else {
        return ConstructRefValue(isolate, x);
      }
    } else {
      return ConstructRefValue(isolate, x);
    }
  }
};

template <typename T>
struct FromImmRefCnt<std::shared_ptr<const T>> {
  using type = Ref<typename FromImmRefCnt<T>::type>;
};

template <typename T>
struct TypeScriptTypeString<std::shared_ptr<const T>> {
  static std::string value() { return TypeScriptTypeString<T>::value(); }
};

template <typename T>
struct TypeMatchesTypeScriptString<std::shared_ptr<const T>> {
  static bool Result(std::string_view ts) {
    return TypeMatchesTypeScriptString<T>::Result(ts);
  }
};

template <typename T>
struct FromImmRefCnt<reify::CachedHashReference<T>> {
  using type = Ref<typename FromImmRefCnt<T>::type>;
};

template <typename T>
struct TypeScriptTypeString<reify::CachedHashReference<T>> {
  static std::string value() { return TypeScriptTypeString<T>::value(); }
};

template <typename T>
struct TypeMatchesTypeScriptString<reify::CachedHashReference<T>> {
  static bool Result(std::string_view ts) {
    return TypeMatchesTypeScriptString<T>::Result(ts);
  }
};
}  // namespace reify_v8

#endif  // _REIFY_IDT_TARGETS_TYPESCRIPT_CPP_V8_COMMON_TYPES_H_
