#include <libplatform/libplatform.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <v8.h>

#include <cassert>
#include <fstream>
#include <iostream>
#include <sstream>

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
}  // namespace

void ProcessResult(v8::Isolate* isolate, const v8::Local<v8::Value> result) {
  auto mesh3 = v8::Local<reify::Mesh3>::Cast(result);

  std::cout << "Kind: " << mesh3->sub_type() << std::endl;

  switch (mesh3->sub_type()) {
    case reify::Mesh3::ExtrudeMesh2AsMesh:
      std::cout << "ExtrudeMesh2AsMesh" << std::endl;
      break;
    case reify::Mesh3::TransformMesh3AsMesh:
      std::cout << "TransformMesh3AsMesh" << std::endl;
      break;
    case reify::Mesh3::MeshUnion:
      std::cout << "MeshUnion" << std::endl;
      auto meshes = mesh3->AsMeshUnion().ToLocalChecked();
      std::cout << "  Number of meshes: " << meshes->Length() << std::endl;
      std::cout << "  [" << std::endl;
      for (int i = 0; i < meshes->Length(); ++i) {
        ProcessResult(isolate, meshes->Get(i).ToLocalChecked());
      }
      std::cout << "  ]" << std::endl;
  }
}

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
    std::string interface_augmented_input_src = reify_ts_interface_src_str +
                                                ts_lib_reify_str + "\n" +
                                                LoadFile(argv[1]);

    TypeScriptCompiler tsc;
    std::string input_file_contents =
        tsc.TranspileToJavaScript(interface_augmented_input_src.c_str());
    std::cout << input_file_contents << std::endl << std::endl;

    // Create a new Isolate and make it the current one.
    v8::Isolate::CreateParams create_params;
    create_params.array_buffer_allocator =
        v8::ArrayBuffer::Allocator::NewDefaultAllocator();
    v8::Isolate* isolate = v8::Isolate::New(create_params);
    {
      v8::Isolate::Scope isolate_scope(isolate);
      // Create a stack-allocated handle scope.
      v8::HandleScope handle_scope(isolate);
      // Create a new context.
      v8::Local<v8::Context> context = v8::Context::New(isolate);
      // Enter the context for compiling and running the hello world script.
      v8::Context::Scope context_scope(context);

      // We're just about to compile the script; set up an error handler to
      // catch any exceptions the script might throw.
      v8::TryCatch try_catch(isolate);

      // Create a string containing the JavaScript source code.
      v8::Local<v8::String> source;
      if (!v8::String::NewFromUtf8(isolate, input_file_contents.c_str(),
                                   v8::NewStringType::kNormal)
               .ToLocal(&source)) {
        v8::String::Utf8Value error(isolate, try_catch.Exception());
        std::cerr << "Error: " << *error << std::endl;
        return 1;
      }

      // Compile the source code.
      v8::Local<v8::Script> script;
      if (!v8::Script::Compile(context, source).ToLocal(&script)) {
        v8::String::Utf8Value error(isolate, try_catch.Exception());
        std::cerr << "Error: " << *error << std::endl;
        return 1;
      }

      // Run the script to get the result.
      v8::Local<v8::Value> global_result;
      if (!script->Run(context).ToLocal(&global_result)) {
        v8::String::Utf8Value error(isolate, try_catch.Exception());
        std::cerr << "Error: " << *error << std::endl;
        return 1;
      }

      // The script compiled and ran correctly.  Now we fetch out the
      // Process function from the global object.
      v8::Local<v8::String> function_name =
          v8::String::NewFromUtf8(isolate, "Jeep", v8::NewStringType::kNormal)
              .ToLocalChecked();

      // If there is no Process function, or if it is not a function,
      // bail out.
      v8::Local<v8::Value> function_untyped_val;
      if (!context->Global()
               ->Get(context, function_name)
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
