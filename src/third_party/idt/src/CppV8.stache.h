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

template <typename T>
struct FromImmRefCnt {};

// Parses a TypeScript string and returns true if it matches the passed in
// C++ string.
template <typename T>
struct TypeMatchesTypeScriptString {};

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

  int32_t Value() const {
    return static_cast<int32_t>(v8::Number::Value());
  }

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

  float Value() const {
    return static_cast<float>(v8::Number::Value());
  }

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
      {{namespace}}::Value(isolate, x->Get(Indices))...};
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

template <typename T>
class Ref : public T {
 public:
  using DerefValueType = const decltype(
      Value(std::declval<v8::Isolate*>(), std::declval<v8::Local<T>>()));
  using RefValueType = std::shared_ptr<DerefValueType>;

  V8_INLINE static Ref<T>* Cast(v8::Value* obj) {
    return static_cast<Ref<T>*>(T::Cast(obj));
  }
};

template <typename T>
auto ConstructRefValue(v8::Isolate* isolate, v8::Local<Ref<T>> x) {
  return std::make_shared<typename Ref<T>::DerefValueType>(
      Value(isolate, v8::Local<T>::Cast(x)));
}

template <typename T>
struct WeakCallbackData {
  // Unfortunately, we can't setup a floating callback without also keeping
  // a v8::Persistent object alive, so we bundle it here with the data.
  v8::Persistent<Ref<T>> persistent;
  typename Ref<T>::RefValueType ptr_data;
};

template <typename T>
auto Value(v8::Isolate* isolate, v8::Local<Ref<T>> x) {
  using RefValueType = typename Ref<T>::RefValueType;
  if constexpr (std::is_base_of<v8::Object, T>::value) {
    if (x->InternalFieldCount() > 0) {
      // If we have an internal field (i.e. we were created from an object
      // template), cache the returned shared_ptr value into the internal field
      // and then return that.
      auto internal_field = static_cast<WeakCallbackData<T>*>(
          x->GetAlignedPointerFromInternalField(0));
      if (!internal_field) {
        internal_field =
            new WeakCallbackData<T>
                {.persistent = v8::Persistent<Ref<T>>(isolate, x),
                  .ptr_data = RefValueType(ConstructRefValue(isolate, x))};

        x->SetAlignedPointerInInternalField(0, internal_field);

        // We package up a pointer to the Persistent<Ref<T>> object itself into
        // the callback so that we can reset it after it has been cleaned up,
        // as the V8 API requires.
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

template <typename T>
struct FromImmRefCnt<std::shared_ptr<const T>> {
  using type = Ref<typename FromImmRefCnt<T>::type>;
};

template <typename T>
struct TypeMatchesTypeScriptString<std::shared_ptr<const T>> {
  static bool Result(std::string_view ts) {
    return TypeMatchesTypeScriptString<T>::Result(ts);
  }
};

{{#declarationSequence}}
{{{.}}}
{{/declarationSequence}}

}  // {{namespace}}

#endif  // _{{namespace}}_CPP_V8_IST_GENERATED_H_