#include "public_include/reify.h"

namespace REIFY_GENERATED_PROJECT_NAMESPACE {
namespace reify {

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

}  // namespace reify
}  // namespace REIFY_GENERATED_PROJECT_NAMESPACE
