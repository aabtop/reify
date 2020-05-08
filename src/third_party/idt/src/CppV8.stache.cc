// clang-format off
#include "{{v8HeaderFile}}"
#include "{{immutableRefCountedHeaderFile}}"

#include <v8.h>

namespace {{namespace}} {

namespace {

static v8::Local<v8::Value> Throw(v8::Isolate* isolate, const char* message) {
  return isolate->ThrowException(v8::String::NewFromUtf8(isolate, message));
}

{{#constructorDefinitions}}
void New{{name}}(const v8::FunctionCallbackInfo<v8::Value>& args) {
  v8::Isolate* isolate = args.GetIsolate();
  v8::HandleScope handle_scope(isolate);
  v8::Local<v8::Context> context = isolate->GetCurrentContext();

  if (args.Length() < 1 || !args[0]->IsObject()) {
    Throw(isolate, "Expected an object for the first argument.");
    return;
  }

  v8::Local<v8::Object> return_value = args[0].As<v8::Object>()->Clone();

  {{immRefCntNamespace}}::{{name}}::Data data_object;
  // Populate the data_object with the values from V8.

  auto internal_object_ptr =
      new std::shared_ptr<{{immRefCntNamespace}}::{{name}}>(
          {{immRefCntNamespace}}::{{name}}::make_shared(
              std::move(data_object)));

  return_value->SetInternalField(
      0, v8::External::New(isolate, internal_object_ptr));

  args.GetReturnValue().Set(return_value);
}
{{/constructorDefinitions}}

}  // namespace

void InstallInterfaceToGlobalObject(
    v8::Isolate* isolate, v8::Local<v8::ObjectTemplate> global_template) {
{{#constructorDefinitions}}
  global_template->Set(isolate, "New{{name}}", v8::FunctionTemplate::New(isolate, New{{name}}));
{{/constructorDefinitions}}  
}

}  // {{namespace}}

