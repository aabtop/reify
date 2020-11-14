// {{!
// clang-format off
// }}
#ifndef _{{namespace}}_CPP_V8_IST_GENERATED_H_
#define _{{namespace}}_CPP_V8_IST_GENERATED_H_

#include <cassert>
#include <tuple>
#include <vector>

#include <v8.h>

#include "reify/common_types.h"
#include "reify/typescript_cpp_v8.h"

#include "{{immutableRefCountedHeaderFile}}"

namespace reify_v8 {

{{#declarationSequence}}
{{{.}}}
{{/declarationSequence}}


template <typename T>
class Ref : public T {
 public:
  using DerefValueType = const decltype(reify_v8::Value(
      std::declval<v8::Isolate*>(), std::declval<v8::Local<T>>()));
  using RefValueType = std::shared_ptr<DerefValueType>;

  V8_INLINE static Ref<T>* Cast(v8::Value* obj) {
    return static_cast<Ref<T>*>(T::Cast(obj));
  }
};

template <typename T>
typename Ref<T>::RefValueType ConstructRefValue(v8::Isolate* isolate,
                                                v8::Local<Ref<T>> x) {
  return std::make_shared<typename Ref<T>::DerefValueType>(
      Value(isolate, v8::Local<T>::Cast(x)));
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
auto Value(v8::Isolate* isolate, v8::Local<Ref<T>> x) {
  if constexpr (std::is_base_of<v8::Object, T>::value) {
    if (x->InternalFieldCount() > 0) {
      // If we have an internal field (i.e. we were created from an object
      // template), cache the returned shared_ptr value into the internal field
      // and then return that.
      auto internal_field = static_cast<WeakCallbackData<T>*>(
          x->GetAlignedPointerFromInternalField(0));
      if (!internal_field) {
        internal_field = new WeakCallbackData<T>(isolate, x);

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

}  // namespace reify_v8

namespace reify {

std::vector<CompilerEnvironment::InputModule>
    {{namespace}}_typescript_declarations();

}  // namespace reify

#endif  // _{{namespace}}_CPP_V8_IST_GENERATED_H_