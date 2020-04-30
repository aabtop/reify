// clang-format off
#include <v8.h>

namespace {{namespace}} {

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

{{#declarationSequence}}
{{.}}
{{/declarationSequence}}

}  // {{namespace}}

