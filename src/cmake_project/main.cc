#include <libplatform/libplatform.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <v8.h>

#include <cassert>
#include <fstream>
#include <iostream>
#include <sstream>
#include <variant>

#include "context_environment.h"
#include "src_gen/interface/cpp/reify_cpp_immut_ref_counted_interface.h"
#include "src_gen/interface/cpp/reify_cpp_v8_interface.h"
#include "typescript_compiler.h"

namespace {
#include "src_gen/reify_generated_interface_ts.h"
#include "src_gen/reify_interface_ts.h"
}  // namespace

namespace {
std::string ToStdString(v8::Isolate* isolate, const v8::Local<v8::Value> str) {
  v8::String::Utf8Value utf8_kind_value(isolate, str.template As<v8::String>());

  return std::string(*utf8_kind_value, utf8_kind_value.length());
}

std::string LoadFile(const char* filename) {
  std::ifstream in(filename);
  std::stringstream buffer;
  buffer << in.rdbuf();
  return buffer.str();
}

v8::MaybeLocal<v8::Module> ResolveModuleCallback(
    v8::Local<v8::Context> context, v8::Local<v8::String> specifier,
    v8::Local<v8::Module> referrer);

v8::MaybeLocal<v8::Module> InstantiateModule(
    v8::Local<v8::Context> context, const TypeScriptCompiler::Module& module) {
  auto isolate = context->GetIsolate();
  auto source_text = v8::String::NewFromUtf8(isolate, module.content.c_str());

  v8::ScriptOrigin origin(
      v8::String::NewFromUtf8(isolate,
                              module.path.c_str()),  // specifier
      v8::Integer::New(isolate, 0),                  // line offset
      v8::Integer::New(isolate, 0),                  // column offset
      False(isolate),                                // is cross origin
      v8::Local<v8::Integer>(),                      // script id
      v8::Local<v8::Value>(),                        // source map URL
      v8::False(isolate),                            // is opaque
      v8::False(isolate),                            // is WASM
      v8::True(isolate));                            // is ES6 module
  v8::ScriptCompiler::Source source(source_text, origin);
  v8::Local<v8::Module> v8_module;
  if (!v8::ScriptCompiler::CompileModule(isolate, &source)
           .ToLocal(&v8_module)) {
    return v8::MaybeLocal<v8::Module>();
  }

  if (v8_module->InstantiateModule(context, &ResolveModuleCallback)
          .IsNothing()) {
    return v8::MaybeLocal<v8::Module>();  //
  }

  return v8_module;
}

v8::MaybeLocal<v8::Module> ResolveModuleCallback(
    v8::Local<v8::Context> context, v8::Local<v8::String> specifier,
    v8::Local<v8::Module> referrer) {
  std::cerr << "ResolveModuleCallback("
            << ToStdString(context->GetIsolate(), specifier) << ")"
            << std::endl;
  const auto transpile_results =
      reinterpret_cast<ContextEnvironment*>(
          context->GetAlignedPointerFromEmbedderData(1))
          ->transpile_results;

  std::string specifier_as_str = ToStdString(context->GetIsolate(), specifier);

  auto module = transpile_results->LookupPath("/" + specifier_as_str + ".js");

  if (!module) {
    std::string error_str =
        "Could not locate module \"" + specifier_as_str + "\".";
    context->GetIsolate()->ThrowException(
        v8::String::NewFromUtf8(context->GetIsolate(), error_str.c_str()));
    return v8::MaybeLocal<v8::Module>();
  }

  return InstantiateModule(context, *module);
}

std::variant<v8::Local<v8::Module>, std::string> EvaluateModules(
    v8::Local<v8::Context> context) {
  auto isolate = context->GetIsolate();
  v8::Context::Scope context_scope(context);
  v8::TryCatch try_catch(isolate);

  const auto transpile_results =
      reinterpret_cast<ContextEnvironment*>(
          context->GetAlignedPointerFromEmbedderData(1))
          ->transpile_results;

  v8::Local<v8::Module> module;
  if (!InstantiateModule(context, transpile_results->GetPrimaryModule())
           .ToLocal(&module)) {
    v8::String::Utf8Value error(isolate, try_catch.Exception());
    return *error;
  }

  v8::Local<v8::Value> evaluate_result;
  if (!module->Evaluate(context).ToLocal(&evaluate_result)) {
    v8::String::Utf8Value error(isolate, try_catch.Exception());
    return *error;
  }

  return module;
}

void ProcessResult(const hypo::Mesh3& mesh3) {
  if (auto extrude = std::get_if<std::shared_ptr<hypo::ExtrudeMesh2>>(&mesh3)) {
    std::cout << "ExtrudeMesh2AsMesh" << std::endl;
  } else if (auto transform =
                 std::get_if<std::shared_ptr<hypo::TransformMesh3>>(&mesh3)) {
    std::cout << "TransformMesh3AsMesh" << std::endl;
  } else if (auto mesh_union =
                 std::get_if<std::shared_ptr<hypo::Mesh3Union>>(&mesh3)) {
    std::cout << "MeshUnion" << std::endl;
    std::cout << "  Number of meshes: " << (*mesh_union)->meshes.size()
              << std::endl;
    std::cout << "  [" << std::endl;
    for (const auto& mesh : (*mesh_union)->meshes) {
      ProcessResult(mesh);
    }
    std::cout << "  ]" << std::endl;
  }
}

void WithInternalField(const v8::FunctionCallbackInfo<v8::Value>& args) {
  v8::Isolate* isolate = args.GetIsolate();
  v8::HandleScope handle_scope(isolate);
  v8::Local<v8::Context> context = isolate->GetCurrentContext();

  auto blank_object_with_internal_field =
      reinterpret_cast<ContextEnvironment*>(
          context->GetAlignedPointerFromEmbedderData(1))
          ->blank_object_with_internal_field.Get(isolate);

  if (args.Length() < 1 || !args[0]->IsObject()) {
    isolate->ThrowException(v8::String::NewFromUtf8(
        isolate, "Expected an object for the first argument."));
    return;
  }
  v8::Local<v8::Object> source_object = args[0].As<v8::Object>();

  v8::Local<v8::Object> return_value =
      blank_object_with_internal_field->NewInstance(context).ToLocalChecked();

  // Copy over the properties from the source object to the return object under
  // construction.
  v8::Local<v8::Array> source_property_names =
      source_object->GetOwnPropertyNames(context).ToLocalChecked();
  for (int i = 0; i < source_property_names->Length(); ++i) {
    auto property_name = source_property_names->Get(context, i)
                             .ToLocalChecked()
                             .As<v8::String>();
    return_value
        ->Set(context, property_name,
              source_object->Get(context, property_name).ToLocalChecked())
        .Check();
  }

  return_value->SetAlignedPointerInInternalField(0, nullptr);
  args.GetReturnValue().Set(return_value);
}

void InstallRootFunctions(v8::Isolate* isolate,
                          v8::Local<v8::ObjectTemplate> global_template) {
  global_template->Set(isolate, "withInternalField",
                       v8::FunctionTemplate::New(isolate, WithInternalField));
}
}  // namespace

int main(int argc, char* argv[]) {
  if (argc < 2) {
    std::cerr << "USAGE: " << argv[0] << " INPUT_FILE" << std::endl;
    return 1;
  }

  std::string reify_ts_interface_src_str(
      reinterpret_cast<const char*>(ts_reify_ts_interface_ts),
      ts_reify_ts_interface_ts_len);
  std::string ts_lib_reify_str(reinterpret_cast<const char*>(ts_lib_ts),
                               ts_lib_ts_len);

  // Initialize V8.
  v8::V8::InitializeICUDefaultLocation(argv[0]);
  std::unique_ptr<v8::Platform> platform = v8::platform::NewDefaultPlatform();
  v8::V8::InitializePlatform(platform.get());
  v8::V8::Initialize();

  {
    TypeScriptCompiler tsc;
    auto transpile_results_or_error = tsc.TranspileToJavaScript(
        argv[1], LoadFile(argv[1]).c_str(),
        {.system_modules = {
             {"/reify_generated_interface.ts", reify_ts_interface_src_str},
             {"/" REIFY_INTERFACE_NAMESPACE ".ts", ts_lib_reify_str}}});
    if (auto error = std::get_if<TypeScriptCompiler::Error>(
            &transpile_results_or_error)) {
      std::cerr << "Error compiling TypeScript:" << std::endl;
      std::cerr << error->path << ":" << error->line + 1 << ":"
                << error->column + 1 << ": error: " << error->message
                << std::endl;
      return 1;
    }
    auto transpile_results = std::get<TypeScriptCompiler::TranspileResults>(
        transpile_results_or_error);

    std::cerr << "Successful transpilation." << std::endl;
    std::cerr << "Primary module name: " << transpile_results.primary_module
              << std::endl;
    std::cerr << "Modules:" << std::endl;
    std::cerr << std::endl;
    for (const auto& module : transpile_results.modules) {
      std::cerr << "  Path: " << module.path << std::endl;
      std::cerr << "    Contents: " << module.content << std::endl;
      std::cerr << std::endl;
    }

    // Create a new Isolate and make it the current one.
    v8::Isolate::CreateParams create_params;
    create_params.array_buffer_allocator =
        v8::ArrayBuffer::Allocator::NewDefaultAllocator();
    v8::Isolate* isolate = v8::Isolate::New(create_params);
    {
      v8::Isolate::Scope isolate_scope(isolate);
      // Create a stack-allocated handle scope.
      v8::HandleScope handle_scope(isolate);

      auto blank_object_with_internal_field = v8::ObjectTemplate::New(isolate);
      blank_object_with_internal_field->SetInternalFieldCount(1);

      // Create a template for the global object that contains functions
      // from the generated interface.
      v8::Local<v8::ObjectTemplate> global_template =
          v8::ObjectTemplate::New(isolate);
      InstallRootFunctions(isolate, global_template);

      ContextEnvironment context_environment{
          .transpile_results = &transpile_results,
          .blank_object_with_internal_field =
              v8::Persistent<v8::ObjectTemplate>(
                  isolate, blank_object_with_internal_field),
      };

      // Create a new context.
      v8::Local<v8::Context> context =
          v8::Context::New(isolate, nullptr, global_template);
      context->SetAlignedPointerInEmbedderData(1, &context_environment);

      // Enter the context for compiling and running the hello world script.
      v8::Context::Scope context_scope(context);

      // We're just about to compile the script; set up an error handler to
      // catch any exceptions the script might throw.
      v8::TryCatch try_catch(isolate);

      auto evaluate_module_result = EvaluateModules(context);
      if (auto error = std::get_if<std::string>(&evaluate_module_result)) {
        std::cerr << "Error evaluating: " << *error << std::endl;
        return 1;
      }

      v8::Local<v8::Module> root_module =
          std::get<v8::Local<v8::Module>>(evaluate_module_result);

      v8::Local<v8::Object> root_namespace =
          root_module->GetModuleNamespace().As<v8::Object>();

      // The script compiled and ran correctly.  Now we fetch out the
      // Process function from the global object.
      // If there is no Process function, or if it is not a function,
      // bail out.
      v8::Local<v8::Value> function_untyped_val;
      if (!root_namespace
               ->Get(context, v8::String::NewFromUtf8(isolate, "Jeep"))
               .ToLocal(&function_untyped_val) ||
          !function_untyped_val->IsFunction()) {
        std::cerr << "Could not find function 'Jeep'." << std::endl;
        return 1;
      }

      // It is a function; cast it to a Function
      v8::Local<v8::Function> entrypoint =
          v8::Local<v8::Function>::Cast(function_untyped_val);

      // Call the entrypoint function.
      v8::Local<v8::Value> result;
      if (!entrypoint->Call(context, context->Global(), 0, nullptr)
               .ToLocal(&result)) {
        v8::String::Utf8Value error(isolate, try_catch.Exception());
        std::cerr << "Error: " << *error << std::endl;
        return 1;
      }

      auto mesh3 = v8::Local<hypo_v8::Mesh3>::Cast(result);

      ProcessResult(hypo_v8::Value(isolate, mesh3));

      if (!entrypoint->Call(context, context->Global(), 0, nullptr)
               .ToLocal(&result)) {
        v8::String::Utf8Value error(isolate, try_catch.Exception());
        std::cerr << "Error: " << *error << std::endl;
        return 1;
      }
      mesh3 = v8::Local<hypo_v8::Mesh3>::Cast(result);
      ProcessResult(hypo_v8::Value(isolate, mesh3));
    }

    // Dispose the isolate and tear down V8.
    isolate->LowMemoryNotification();
    isolate->Dispose();
    delete create_params.array_buffer_allocator;
  }

  v8::V8::Dispose();
  v8::V8::ShutdownPlatform();
  return 0;
}
