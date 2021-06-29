#include "public_include/reify/typescript_cpp_v8/typescript_cpp_v8.h"

namespace reify {
namespace typescript_cpp_v8 {

class GenericFunction::Impl {
 public:
  Impl(v8::Local<v8::Context> context, v8::Local<v8::Function> function);
  ~Impl();

  std::variant<RuntimeException, GenericFunction::CallContext> Call(
      int argc, v8::Local<v8::Value> argv[]);

 private:
  v8::Isolate* isolate_;
  v8::Persistent<v8::Context> context_;
  v8::Persistent<v8::Function> function_;

  friend struct GenericFunction::CallContext;
};

}  // namespace typescript_cpp_v8
}  // namespace reify
