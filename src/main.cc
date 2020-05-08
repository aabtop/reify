#include <libplatform/libplatform.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <v8.h>

#include <cassert>
#include <fstream>
#include <iostream>
#include <sstream>

#include "src_gen/reify_interface/reify_cpp_immut_ref_counted_interface.h"
#include "src_gen/reify_interface/reify_cpp_v8_interface.h"
#include "typescript_compiler.h"

namespace {
#include "src_gen/reify_interface/reify_ts_interface.ts.h"
#include "src_gen/ts_lib_reify.ts.h"
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
    return v8::MaybeLocal<v8::Module>();
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
      reinterpret_cast<TypeScriptCompiler::TranspileResults*>(
          context->GetAlignedPointerFromEmbedderData(1));

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
    v8::Local<v8::Context> context,
    const TypeScriptCompiler::TranspileResults& transpile_results) {
  context->SetAlignedPointerInEmbedderData(
      1, const_cast<TypeScriptCompiler::TranspileResults*>(&transpile_results));
  auto isolate = context->GetIsolate();
  v8::Context::Scope context_scope(context);
  v8::TryCatch try_catch(isolate);

  v8::Local<v8::Module> module;
  if (!InstantiateModule(context, transpile_results.GetPrimaryModule())
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

void ProcessResult(v8::Isolate* isolate, const v8::Local<v8::Value> result) {
  auto mesh3 = v8::Local<reify_v8::Mesh3>::Cast(result);

  std::cout << "Kind: " << mesh3->kind() << std::endl;

  switch (mesh3->kind()) {
    case reify_v8::Mesh3::Kind_ExtrudeMesh2:
      std::cout << "ExtrudeMesh2AsMesh" << std::endl;
      break;
    case reify_v8::Mesh3::Kind_TransformMesh3:
      std::cout << "TransformMesh3AsMesh" << std::endl;
      break;
    case reify_v8::Mesh3::Kind_Mesh3Union:
      std::cout << "MeshUnion" << std::endl;
      auto mesh_union = mesh3->AsMesh3Union().ToLocalChecked();
      std::cout << "  Number of meshes: " << mesh_union->meshes()->Length()
                << std::endl;
      std::cout << "  [" << std::endl;
      for (int i = 0; i < mesh_union->meshes()->Length(); ++i) {
        ProcessResult(isolate, mesh_union->meshes()->Get(i).ToLocalChecked());
      }
      std::cout << "  ]" << std::endl;
  }
}

// Just a quick little test function to make sure that this is all working.
reify::Mesh3 Cylinder(float radius, float thickness) {
  return reify::ExtrudeMesh2::make_shared(
      {.source =
           reify::Circle::make_shared({.radius = radius, .center = {0, 0}}),
       .path = {{0, 0, -thickness * 0.5f}, {0, 0, thickness * 0.5f}}});
}
}  // namespace

int main(int argc, char* argv[]) {
  if (argc < 2) {
    std::cerr << "USAGE: " << argv[0] << " INPUT_FILE" << std::endl;
    return 1;
  }

  std::string reify_ts_interface_src_str(
      reinterpret_cast<const char*>(reify_ts_interface_src),
      sizeof(reify_ts_interface_src));
  std::string ts_lib_reify_str(reinterpret_cast<const char*>(ts_lib_reify_src),
                               sizeof(ts_lib_reify_src));

  // Initialize V8.
  v8::V8::InitializeICUDefaultLocation(argv[0]);
  v8::V8::InitializeExternalStartupData(argv[0]);
  std::unique_ptr<v8::Platform> platform = v8::platform::NewDefaultPlatform();
  v8::V8::InitializePlatform(platform.get());
  v8::V8::Initialize();

  {
    TypeScriptCompiler tsc;
    auto transpile_results_or_error = tsc.TranspileToJavaScript(
        argv[1], LoadFile(argv[1]).c_str(),
        {.system_modules = {
             {"/reify_core_interface.ts", reify_ts_interface_src_str},
             {"/reify.ts", ts_lib_reify_str}}});
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

      // Create a template for the global object that contains functions
      // from the generated interface.
      v8::Local<v8::ObjectTemplate> global_template =
          v8::ObjectTemplate::New(isolate);
      reify_v8::InstallInterfaceToGlobalObject(isolate, global_template);

      // Create a new context.
      v8::Local<v8::Context> context =
          v8::Context::New(isolate, nullptr, global_template);

      // Enter the context for compiling and running the hello world script.
      v8::Context::Scope context_scope(context);

      // We're just about to compile the script; set up an error handler to
      // catch any exceptions the script might throw.
      v8::TryCatch try_catch(isolate);

      auto evaluate_module_result = EvaluateModules(context, transpile_results);
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

      ProcessResult(isolate, result);
    }

    // Dispose the isolate and tear down V8.
    isolate->Dispose();
    delete create_params.array_buffer_allocator;
  }

  v8::V8::Dispose();
  v8::V8::ShutdownPlatform();
  return 0;
}
