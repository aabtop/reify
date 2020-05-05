#include "typescript_compiler.h"

#include <cassert>
#include <fstream>
#include <iostream>
#include <iterator>
#include <sstream>
#include <string>

namespace {
#include "src_gen/typescript_compiler_js.h"
}  // namespace

namespace {
std::string LoadFile(const char* filename) {
  std::ifstream in(filename);
  std::stringstream buffer;
  buffer << in.rdbuf();
  return buffer.str();
}
}  // namespace

std::string ToStdString(v8::Isolate* isolate, const v8::Local<v8::Value> str) {
  v8::String::Utf8Value utf8_kind_value(isolate, str.template As<v8::String>());

  return std::string(*utf8_kind_value, utf8_kind_value.length());
}

TypeScriptCompiler::TypeScriptCompiler() {
  // Create a new Isolate and make it the current one.
  isolate_create_params_.array_buffer_allocator =
      v8::ArrayBuffer::Allocator::NewDefaultAllocator();
  isolate_ = v8::Isolate::New(isolate_create_params_);

  v8::Isolate::Scope isolate_scope(isolate_);

  // Create a stack-allocated handle scope.
  v8::HandleScope handle_scope(isolate_);

  // Create a new context.
  context_.Reset(isolate_, v8::Context::New(isolate_));

  auto context = v8::Local<v8::Context>::New(isolate_, context_);

  // Enter the context for compiling and running the hello world script.
  v8::Context::Scope context_scope(context);

  // We're just about to compile the script; set up an error handler to
  // catch any exceptions the script might throw.
  v8::TryCatch try_catch(isolate_);

  // Create a string containing the JavaScript source code.
  v8::Local<v8::String> source_tsc;
  if (!v8::String::NewFromUtf8(
           isolate_, reinterpret_cast<const char*>(typescript_compiler_js),
           v8::NewStringType::kNormal, sizeof(typescript_compiler_js))
           .ToLocal(&source_tsc)) {
    v8::String::Utf8Value error(isolate_, try_catch.Exception());
    std::cerr << "Error: " << *error << std::endl;
    assert(false);
  }

  // Compile the source code.
  v8::Local<v8::Script> script_tsc;
  if (!v8::Script::Compile(context, source_tsc).ToLocal(&script_tsc)) {
    v8::String::Utf8Value error(isolate_, try_catch.Exception());
    std::cerr << "Error: " << *error << std::endl;
    assert(false);
  }

  // Run the script to get the result.
  v8::Local<v8::Value> global_result_tsc;
  if (!script_tsc->Run(context).ToLocal(&global_result_tsc)) {
    v8::String::Utf8Value error(isolate_, try_catch.Exception());
    std::cerr << "Error: " << *error << std::endl;
    assert(false);
  }

  v8::Local<v8::String> namespace_name =
      v8::String::NewFromUtf8(isolate_, "tsc_embedded",
                              v8::NewStringType::kNormal)
          .ToLocalChecked();
  auto tsc_embedded_object = context->Global()
                                 ->Get(context, namespace_name)
                                 .ToLocalChecked()
                                 .As<v8::Object>();
  assert(tsc_embedded_object->IsObject());

  // The script compiled and ran correctly.  Now we fetch out the
  // Process function from the global object.
  v8::Local<v8::String> function_name =
      v8::String::NewFromUtf8(isolate_, "TranspileModule",
                              v8::NewStringType::kNormal)
          .ToLocalChecked();

  // If there is no Process function, or if it is not a function,
  // bail out.
  auto transpile_function = tsc_embedded_object->Get(context, function_name)
                                .ToLocalChecked()
                                .As<v8::Function>();
  assert(transpile_function->IsFunction());
  // It is a function; cast it to a Function
  transpile_function_.Reset(isolate_, transpile_function);
}

TypeScriptCompiler::~TypeScriptCompiler() {
  transpile_function_.Reset();
  context_.Reset();
  isolate_->Dispose();
  delete isolate_create_params_.array_buffer_allocator;
}

std::string TypeScriptCompiler::TranspileToJavaScript(
    const char* input_typescript) {
  v8::Isolate::Scope isolate_scope(isolate_);
  v8::HandleScope handle_scope(isolate_);
  auto context = v8::Local<v8::Context>::New(isolate_, context_);
  v8::Context::Scope context_scope(context);
  v8::TryCatch try_catch(isolate_);

  auto transpile_function =
      v8::Local<v8::Function>::New(isolate_, transpile_function_);

  v8::Local<v8::String> input_typescript_v8_str =
      v8::String::NewFromUtf8(isolate_, input_typescript,
                              v8::NewStringType::kNormal)
          .ToLocalChecked();

  v8::Local<v8::Value> parameters[] = {input_typescript_v8_str};
  v8::Local<v8::Object> global = context->Global();
  auto result = transpile_function->Call(context, global, 1, parameters)
                    .ToLocalChecked()
                    .As<v8::Object>();

  v8::Local<v8::String> output_text_name =
      v8::String::NewFromUtf8(isolate_, "result", v8::NewStringType::kNormal)
          .ToLocalChecked();
  auto output_text =
      result->Get(context, output_text_name).ToLocalChecked().As<v8::String>();

  return ToStdString(isolate_, output_text);
}
