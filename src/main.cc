#include <libplatform/libplatform.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <v8.h>

#include <cassert>
#include <fstream>
#include <iostream>
#include <sstream>

class Mesh3;

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

class Mesh3 : public v8::Object {
 public:
  // static Local<Value> New(Isolate* isolate, int64_t value);

  V8_INLINE static Mesh3* Cast(Value* obj) {
#ifdef V8_ENABLE_CHECKS
    CheckCast(obj);
#endif
    return static_cast<Mesh3*>(obj);
  }

  enum SubType {
    ExtrudeMesh2AsMesh,
    TransformMesh3AsMesh,
    MeshUnion,
  };
  SubType sub_type() {
    v8::Local<v8::String> key_name =
        v8::String::NewFromUtf8(GetIsolate(), "__kind");

    v8::String::Utf8Value utf8_kind_value(GetIsolate(),
                                          Get(key_name).As<v8::String>());
    std::string kind_value(*utf8_kind_value, utf8_kind_value.length());

    if (kind_value == "ExtrudeMesh2AsMesh") {
      return ExtrudeMesh2AsMesh;
    } else if (kind_value == "TransformMesh3AsMesh") {
      return TransformMesh3AsMesh;
    } else if (kind_value == "MeshUnion") {
      return MeshUnion;
    } else {
      assert(false);
      return static_cast<SubType>(-1);
    }
  }

  v8::MaybeLocal<List<Mesh3>> AsMeshUnion() {
    if (sub_type() != MeshUnion) {
      return v8::MaybeLocal<List<Mesh3>>();
    }
    v8::Local<v8::String> key_name =
        v8::String::NewFromUtf8(GetIsolate(), "p0");

    return Get(key_name).As<List<Mesh3>>();
  }

 private:
  static void CheckCast(Value* obj) {}
};

void ProcessResult(v8::Isolate* isolate, const v8::Local<v8::Value> result) {
  auto mesh3 = v8::Local<Mesh3>::Cast(result);

  std::cout << "Kind: " << mesh3->sub_type() << std::endl;

  switch (mesh3->sub_type()) {
    case Mesh3::ExtrudeMesh2AsMesh:
      std::cout << "ExtrudeMesh2AsMesh" << std::endl;
      break;
    case Mesh3::TransformMesh3AsMesh:
      std::cout << "TransformMesh3AsMesh" << std::endl;
      break;
    case Mesh3::MeshUnion:
      std::cout << "MeshUnion" << std::endl;
      auto meshes = mesh3->AsMeshUnion().ToLocalChecked();
      std::cout << "  Number of meshes: " << meshes->Length() << std::endl;
      std::cout << "  [" << std::endl;
      for (int i = 0; i < meshes->Length(); ++i) {
        std::cout << "    " << meshes->Get(i).ToLocalChecked()->sub_type()
                  << std::endl;
      }
      std::cout << "  ]" << std::endl;
  }
}

std::string LoadFile(const char* filename) {
  std::ifstream in(filename);
  std::stringstream buffer;
  buffer << in.rdbuf();
  return buffer.str();
}

int main(int argc, char* argv[]) {
  if (argc < 2) {
    std::cerr << "USAGE: " << argv[0] << " INPUT_FILE" << std::endl;
    return 1;
  }

  std::string input_file_contents = LoadFile(argv[1]);

  // Initialize V8.
  v8::V8::InitializeICUDefaultLocation(argv[0]);
  v8::V8::InitializeExternalStartupData(argv[0]);
  std::unique_ptr<v8::Platform> platform = v8::platform::NewDefaultPlatform();
  v8::V8::InitializePlatform(platform.get());
  v8::V8::Initialize();
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

    {
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
  }
  // Dispose the isolate and tear down V8.
  isolate->Dispose();
  v8::V8::Dispose();
  v8::V8::ShutdownPlatform();
  delete create_params.array_buffer_allocator;
  return 0;
}
