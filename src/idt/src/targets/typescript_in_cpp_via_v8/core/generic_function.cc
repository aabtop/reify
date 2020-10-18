#include "generic_function_impl.h"
#include "public_include/reify.h"

namespace REIFY_GENERATED_PROJECT_NAMESPACE {
namespace reify {

GenericFunction::Impl::Impl(v8::Local<v8::Context> context,
                            v8::Local<v8::Function> function)
    : isolate_(context->GetIsolate()),
      context_(isolate_, context),
      function_(isolate_, function) {}

GenericFunction::Impl::~Impl() {
  function_.Reset();
  context_.Reset();
}

GenericFunction::CallContext::CallContext(GenericFunction* function)
    : isolate(function->impl()->isolate_),
      isolate_scope(isolate),
      handle_scope(isolate),
      context(v8::Local<v8::Context>::New(isolate, function->impl()->context_)),
      context_scope(context),
      try_catch(isolate),
      function(
          v8::Local<v8::Function>::New(isolate, function->impl()->function_)) {}

std::variant<RuntimeException, v8::Local<v8::Value>>
GenericFunction::CallContext::Call(int argc, v8::Local<v8::Value> argv[]) {
  // Call the entrypoint function.
  v8::Local<v8::Value> result;
  if (!function->Call(context, context->Global(), 0, nullptr)
           .ToLocal(&result)) {
    v8::String::Utf8Value error(isolate, try_catch.Exception());
    return *error;
  }

  return result;
}

GenericFunction::GenericFunction(std::unique_ptr<Impl> impl)
    : impl_(std::move(impl)) {}
GenericFunction::GenericFunction(GenericFunction&&) = default;
GenericFunction::~GenericFunction() {}

}  // namespace reify
}  // namespace REIFY_GENERATED_PROJECT_NAMESPACE
