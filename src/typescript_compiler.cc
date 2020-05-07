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

auto TypeScriptCompiler::TranspileResults::LookupPath(
    const std::string& path) const -> const Module* {
  for (const auto& m : modules) {
    if (m.path == path) {
      return &m;
    }
  }
  return nullptr;
}

std::string ToStdString(v8::Isolate* isolate, const v8::Local<v8::Value> str) {
  assert(str->IsString());
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

  auto namespace_name = v8::String::NewFromUtf8(isolate_, "tsc_wrapper");
  auto tsc_wrapper_object = context->Global()
                                ->Get(context, namespace_name)
                                .ToLocalChecked()
                                .As<v8::Object>();
  assert(tsc_wrapper_object->IsObject());

  // The script compiled and ran correctly.  Now we fetch out the
  // Process function from the global object.
  auto function_name = v8::String::NewFromUtf8(isolate_, "TranspileModule");

  // If there is no Process function, or if it is not a function,
  // bail out.
  auto transpile_function = tsc_wrapper_object->Get(context, function_name)
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

namespace {
v8::Local<v8::Object> CreateSystemModuleMap(
    v8::Isolate* isolate, v8::Local<v8::Context> context,
    const std::vector<TypeScriptCompiler::Module>& system_modules) {
  v8::Local<v8::Object> system_module_map = v8::Object::New(isolate);
  for (const auto& module : system_modules) {
    auto path_value = v8::String::NewFromUtf8(isolate, module.path.c_str());
    auto content_value =
        v8::String::NewFromUtf8(isolate, module.content.c_str());

    system_module_map->Set(context, path_value, content_value).Check();
  }

  return system_module_map;
}
}  // namespace

auto TypeScriptCompiler::TranspileToJavaScript(const char* input_path,
                                               const char* input_typescript,
                                               const CompileOptions& options)
    -> std::variant<TranspileResults, Error> {
  v8::Isolate::Scope isolate_scope(isolate_);
  v8::HandleScope handle_scope(isolate_);
  auto context = v8::Local<v8::Context>::New(isolate_, context_);
  v8::Context::Scope context_scope(context);
  v8::TryCatch try_catch(isolate_);

  auto system_module_map =
      CreateSystemModuleMap(isolate_, context, options.system_modules);

  auto transpile_function =
      v8::Local<v8::Function>::New(isolate_, transpile_function_);

  auto input_path_v8_str = v8::String::NewFromUtf8(isolate_, input_path);
  auto input_typescript_v8_str =
      v8::String::NewFromUtf8(isolate_, input_typescript);

  v8::Local<v8::Value> parameters[] = {
      input_path_v8_str, input_typescript_v8_str, system_module_map};
  v8::Local<v8::Object> global = context->Global();
  auto result = transpile_function->Call(context, global, 3, parameters)
                    .ToLocalChecked()
                    .As<v8::Object>();
  assert(result->IsObject());

  auto success_field_name = v8::String::NewFromUtf8(isolate_, "success");
  v8::Local<v8::Boolean> success = result->Get(context, success_field_name)
                                       .ToLocalChecked()
                                       .As<v8::Boolean>();
  assert(success->IsBoolean());

  if (!success->Value()) {
    auto error =
        result->Get(context, v8::String::NewFromUtf8(isolate_, "error"))
            .ToLocalChecked()
            .As<v8::Object>();
    assert(error->IsObject());

    auto path = error->Get(context, v8::String::NewFromUtf8(isolate_, "path"))
                    .ToLocalChecked()
                    .As<v8::String>();
    assert(path->IsString());
    auto line = error->Get(context, v8::String::NewFromUtf8(isolate_, "line"))
                    .ToLocalChecked()
                    .As<v8::Number>();
    assert(line->IsNumber());
    auto column =
        error->Get(context, v8::String::NewFromUtf8(isolate_, "column"))
            .ToLocalChecked()
            .As<v8::Number>();
    assert(column->IsNumber());
    auto message =
        error->Get(context, v8::String::NewFromUtf8(isolate_, "message"))
            .ToLocalChecked()
            .As<v8::String>();
    assert(message->IsString());

    return Error{.path = ToStdString(isolate_, path),
                 .line = static_cast<int>(line->Value()),
                 .column = static_cast<int>(column->Value()),
                 .message = ToStdString(isolate_, message)};
  }

  auto output_field_name = v8::String::NewFromUtf8(isolate_, "output");
  v8::Local<v8::Object> output =
      result->Get(context, output_field_name).ToLocalChecked().As<v8::Object>();
  assert(output->IsObject());

  TranspileResults return_value;

  auto primary_module_field_name =
      v8::String::NewFromUtf8(isolate_, "primary_module");
  auto primary_module = output->Get(context, primary_module_field_name)
                            .ToLocalChecked()
                            .As<v8::String>();
  return_value.primary_module = ToStdString(isolate_, primary_module);

  auto js_modules_field_name = v8::String::NewFromUtf8(isolate_, "js_modules");
  v8::Local<v8::Object> js_modules = output->Get(context, js_modules_field_name)
                                         .ToLocalChecked()
                                         .As<v8::Object>();
  assert(js_modules->IsObject());

  v8::Local<v8::Array> module_paths =
      js_modules->GetOwnPropertyNames(context).ToLocalChecked();
  for (int i = 0; i < module_paths->Length(); ++i) {
    auto module_path =
        module_paths->Get(context, i).ToLocalChecked().As<v8::String>();
    std::string module_path_str = ToStdString(isolate_, module_path);
    auto module_contents =
        js_modules->Get(context, module_path).ToLocalChecked().As<v8::String>();
    std::string module_conents_str = ToStdString(isolate_, module_contents);
    return_value.modules.push_back(
        {.path = module_path_str, .content = module_conents_str});
  }

  if (!return_value.LookupPath(return_value.primary_module)) {
    return Error{.path = input_path,
                 .line = 0,
                 .column = 0,
                 .message = "Could not find the primary output module, '" +
                            return_value.primary_module +
                            "', in the emitted results."};
  }

  return return_value;
}
