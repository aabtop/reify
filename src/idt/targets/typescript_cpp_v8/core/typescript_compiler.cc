#include "typescript_compiler.h"

#include <stdlib.h>

#include <cassert>
#include <filesystem>
#include <fstream>
#include <iostream>
#include <iterator>
#include <sstream>
#include <string>

namespace reify {
namespace typescript_cpp_v8 {

namespace {
#include "src_gen/tsc_wrapper.h"
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

namespace {
std::filesystem::path GetCacheFilePath() {
  std::filesystem::path home_dir(getenv("HOME"));
  std::filesystem::path hypo_cache_dir(home_dir / ".cache/hypo");
  return hypo_cache_dir / "typescript_compiler_v8_snapshot";
}

struct TranspileContextEnvironment {
  VirtualFilesystem* virtual_filesystem;
};

VirtualFilesystem* GetVirtualFilesystem(v8::Isolate* isolate) {
  return reinterpret_cast<TranspileContextEnvironment*>(
             isolate->GetCurrentContext()->GetAlignedPointerFromEmbedderData(1))
      ->virtual_filesystem;
}

namespace injected_functions {
void GetSourceFile(const v8::FunctionCallbackInfo<v8::Value>& args) {
  v8::Isolate* isolate = args.GetIsolate();
  v8::HandleScope handle_scope(isolate);

  if (args.Length() < 1 || !args[0]->IsString()) {
    isolate->ThrowException(v8::String::NewFromUtf8Literal(
        isolate, "Expected a string for the first argument."));
    return;
  }

  auto virtual_filepath = *VirtualFilesystem::AbsolutePath::FromString(
      ToStdString(isolate, args[0]));

  auto file = GetVirtualFilesystem(isolate)->GetFile(virtual_filepath);
  auto maybe_file_contents = file.get_content();

  if (auto* error =
          std::get_if<VirtualFilesystem::Error>(&maybe_file_contents)) {
    isolate->ThrowException(
        v8::String::NewFromUtf8(
            isolate, ("Error reading data from " + file.diagnostics_path +
                      ": " + error->message)
                         .c_str())
            .ToLocalChecked());
    return;
  }
  auto& file_contents = std::get<std::string>(maybe_file_contents);

  v8::Local<v8::String> source_content_v8;
  if (!v8::String::NewFromUtf8(isolate, file_contents.data(),
                               v8::NewStringType::kNormal, file_contents.size())
           .ToLocal(&source_content_v8)) {
    // Let the exception pass through.
    return;
  }

  args.GetReturnValue().Set(source_content_v8);
}

void FileExists(const v8::FunctionCallbackInfo<v8::Value>& args) {
  v8::Isolate* isolate = args.GetIsolate();
  v8::HandleScope handle_scope(isolate);

  if (args.Length() < 1 || !args[0]->IsString()) {
    isolate->ThrowException(v8::String::NewFromUtf8Literal(
        isolate, "Expected a string for the first argument."));
    return;
  }

  auto virtual_filepath = *VirtualFilesystem::AbsolutePath::FromString(
      ToStdString(isolate, args[0]));

  bool exists =
      GetVirtualFilesystem(isolate)->GetFile(virtual_filepath).exists();
  args.GetReturnValue().Set(v8::Boolean::New(isolate, exists));
}

}  // namespace injected_functions

// Setup all of the functions for injection into the JavaScript environment.
v8::Local<v8::ObjectTemplate> CreateGlobalTemplate(v8::Isolate* isolate) {
  v8::Local<v8::ObjectTemplate> global_template =
      v8::ObjectTemplate::New(isolate);

  global_template->Set(
      isolate, "externalGetSourceFile",
      v8::FunctionTemplate::New(isolate, injected_functions::GetSourceFile));
  global_template->Set(
      isolate, "externalFileExists",
      v8::FunctionTemplate::New(isolate, injected_functions::FileExists));

  return global_template;
}
}  // namespace

bool TypeScriptCompiler::LoadIsolateFromSnapshot() {
  std::filesystem::path cache_filepath = GetCacheFilePath();
  std::ifstream fin(cache_filepath, std::ios::binary | std::ios::ate);
  if (fin.fail()) {
    return false;
  }

  auto startup_data = std::make_unique<v8::StartupData>();

  startup_data->raw_size = fin.tellg();
  fin.seekg(0, std::ios::beg);

  auto data = std::unique_ptr<char[]>(new char[startup_data->raw_size]);
  if (!fin.read(data.get(), startup_data->raw_size)) {
    std::cerr << "Error reading snapshot file data from cache." << std::endl;
    return false;
  }

  startup_data->data = data.release();
  isolate_create_params_.snapshot_blob = startup_data.release();

  // Create a new Isolate and make it the current one.
  isolate_create_params_.array_buffer_allocator =
      v8::ArrayBuffer::Allocator::NewDefaultAllocator();
  isolate_ = v8::Isolate::New(isolate_create_params_);

  v8::Isolate::Scope isolate_scope(isolate_);
  v8::HandleScope handle_scope(isolate_);

  context_.Reset(isolate_, v8::Context::New(isolate_, nullptr,
                                            CreateGlobalTemplate(isolate_)));

  return true;
}

bool TypeScriptCompiler::CreateAndSaveIsolateToSnapshot() {
  std::filesystem::path cache_filepath = GetCacheFilePath();
  std::filesystem::create_directories(cache_filepath.parent_path());
  std::ofstream fout(cache_filepath, std::ios::binary | std::ios::ate);
  if (fout.fail()) {
    std::cerr << "Error writing to V8 snapshot cache file '" << cache_filepath
              << "'." << std::endl;
    return false;
  }

  v8::SnapshotCreator snapshot_creator;
  v8::Isolate* isolate = snapshot_creator.GetIsolate();
  {
    v8::Isolate::Scope isolate_scope(isolate);
    v8::HandleScope handle_scope(isolate);
    InitializeIsolate(isolate, &snapshot_creator);
  }
  isolate->LowMemoryNotification();

  v8::StartupData startup_data = snapshot_creator.CreateBlob(
      v8::SnapshotCreator::FunctionCodeHandling::kKeep);

  fout.write(startup_data.data, startup_data.raw_size);

  return true;
}

void TypeScriptCompiler::InitializeIsolateWithoutSnapshot() {
  isolate_create_params_.array_buffer_allocator =
      v8::ArrayBuffer::Allocator::NewDefaultAllocator();
  isolate_ = v8::Isolate::New(isolate_create_params_);

  v8::Isolate::Scope isolate_scope(isolate_);
  v8::HandleScope handle_scope(isolate_);
  context_.Reset(isolate_, InitializeIsolate(isolate_, nullptr));
}

v8::Local<v8::Context> TypeScriptCompiler::InitializeIsolate(
    v8::Isolate* isolate, v8::SnapshotCreator* snapshot_creator) {
  // Create a new context.
  v8::Local<v8::Context> context =
      v8::Context::New(isolate, nullptr, CreateGlobalTemplate(isolate));

  // Enter the context for compiling and running the hello world script.
  v8::Context::Scope context_scope(context);

  // We're just about to compile the script; set up an error handler to
  // catch any exceptions the script might throw.
  v8::TryCatch try_catch(isolate);

  // Create a string containing the JavaScript source code.
  v8::Local<v8::String> source_tsc;
  if (!v8::String::NewFromUtf8(isolate,
                               reinterpret_cast<const char*>(tsc_wrapper_js),
                               v8::NewStringType::kNormal, tsc_wrapper_js_len)
           .ToLocal(&source_tsc)) {
    v8::String::Utf8Value error(isolate, try_catch.Exception());
    std::cerr << "Error: " << *error << std::endl;
    assert(false);
  }

  v8::Local<v8::String> resource_name =
      v8::String::NewFromUtf8(isolate, "<tsc_wrapper>",
                              v8::NewStringType::kNormal)
          .ToLocalChecked();
  v8::ScriptOrigin origin(resource_name);

  v8::ScriptCompiler::Source source(source_tsc, origin);

  // Compile the source code.
  v8::Local<v8::Script> script_tsc;
  if (!v8::Script::Compile(context, source_tsc, &origin).ToLocal(&script_tsc)) {
    v8::String::Utf8Value error(isolate, try_catch.Exception());
    std::cerr << "Error: " << *error << std::endl;
    assert(false);
  }

  // Run the script to get the result.
  v8::Local<v8::Value> global_result_tsc;
  if (!script_tsc->Run(context).ToLocal(&global_result_tsc)) {
    v8::String::Utf8Value error(isolate, try_catch.Exception());
    std::cerr << "Error: " << *error << std::endl;
    assert(false);
  }

  if (snapshot_creator) {
    snapshot_creator->SetDefaultContext(context);
  }

  return context;
}

void TypeScriptCompiler::LocateTranspileFunction() {
  v8::Isolate::Scope isolate_scope(isolate_);
  v8::HandleScope handle_scope(isolate_);
  auto context = v8::Local<v8::Context>::New(isolate_, context_);
  v8::Context::Scope context_scope(context);
  v8::TryCatch try_catch(isolate_);

  auto namespace_name = v8::String::NewFromUtf8Literal(isolate_, "tsc_wrapper");
  auto tsc_wrapper_object = context->Global()
                                ->Get(context, namespace_name)
                                .ToLocalChecked()
                                .As<v8::Object>();
  assert(tsc_wrapper_object->IsObject());

  // The script compiled and ran correctly.  Now we fetch out the
  // Process function from the global object.
  auto function_name =
      v8::String::NewFromUtf8Literal(isolate_, "TranspileModule");

  // If there is no Process function, or if it is not a function,
  // bail out.
  auto transpile_function = tsc_wrapper_object->Get(context, function_name)
                                .ToLocalChecked()
                                .As<v8::Function>();
  assert(transpile_function->IsFunction());
  // It is a function; cast it to a Function
  transpile_function_.Reset(isolate_, transpile_function);
}

TypeScriptCompiler::TypeScriptCompiler(VirtualFilesystem* virtual_filesystem,
                                       SnapshotOptions snapshot_options)
    : virtual_filesystem_(virtual_filesystem) {
  switch (snapshot_options) {
    case SnapshotOptions::kCacheSnapshot: {
      if (!LoadIsolateFromSnapshot()) {
        std::cerr << "No TypeScript Compiler V8 snapshot found, creating..."
                  << std::endl;
        if (CreateAndSaveIsolateToSnapshot()) {
          std::cerr << "Done, cached in file '" << GetCacheFilePath() << "'."
                    << std::endl;
          if (!LoadIsolateFromSnapshot()) {
            std::cerr << "Failed to load isolate from snapshot after creating "
                         "snapshot."
                      << std::endl;
            assert(false);
            return;
          }
        } else {
          InitializeIsolateWithoutSnapshot();
        }
      }
    } break;
    case SnapshotOptions::kNoSnapshot: {
      InitializeIsolateWithoutSnapshot();
    } break;
  }
  LocateTranspileFunction();
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
    const std::vector<TypeScriptCompiler::InputModule>& system_modules) {
  v8::Local<v8::Object> system_module_map = v8::Object::New(isolate);
  for (const auto& module : system_modules) {
    std::string module_path_as_string = module.path.string();
    auto path_value =
        v8::String::NewFromUtf8(isolate, module_path_as_string.data(),
                                v8::NewStringType::kNormal,
                                module_path_as_string.size())
            .ToLocalChecked();
    auto content_value = v8::String::NewFromUtf8(isolate, module.content.data(),
                                                 v8::NewStringType::kNormal,
                                                 module.content.size())
                             .ToLocalChecked();

    system_module_map->Set(context, path_value, content_value).Check();
  }

  return system_module_map;
}
}  // namespace

auto TypeScriptCompiler::TranspileToJavaScript(
    const VirtualFilesystem::AbsolutePath& path, const CompileOptions& options)
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

  auto path_as_str = path.string();
  auto input_path_v8_str =
      v8::String::NewFromUtf8(isolate_, path_as_str.data(),
                              v8::NewStringType::kNormal, path_as_str.size())
          .ToLocalChecked();

  std::string diagnostics_path =
      virtual_filesystem_->GetFile(path).diagnostics_path;

  auto maybe_input_typescript =
      virtual_filesystem_->GetFile(path).get_content();
  if (auto error =
          std::get_if<VirtualFilesystem::Error>(&maybe_input_typescript)) {
    return Error{
        diagnostics_path,
        0,
        0,
        "Could not open file: " + error->message,
    };
  }
  auto& input_typescript = std::get<std::string>(maybe_input_typescript);

  auto input_typescript_v8_str =
      v8::String::NewFromUtf8(isolate_, input_typescript.data(),
                              v8::NewStringType::kNormal,
                              input_typescript.size())
          .ToLocalChecked();

  TranspileContextEnvironment context_environment{virtual_filesystem_};
  context->SetAlignedPointerInEmbedderData(1, &context_environment);

  v8::Local<v8::Value> parameters[] = {
      input_path_v8_str, input_typescript_v8_str, system_module_map,
      v8::Boolean::New(isolate_, options.generate_declaration_files)};
  v8::Local<v8::Object> global = context->Global();
  v8::Local<v8::Value> call_return_value;
  if (!transpile_function->Call(context, global, 4, parameters)
           .ToLocal(&call_return_value) ||
      !call_return_value->IsObject()) {
    context->SetAlignedPointerInEmbedderData(1, nullptr);
    auto maybe_stack_trace = try_catch.StackTrace(context);
    if (maybe_stack_trace.IsEmpty()) {
      return Error{
          "Internal Error",
          0,
          0,
          "Unknown JS error while running TypeScript compiler.",
      };

    } else {
      v8::String::Utf8Value error_string(
          isolate_, try_catch.StackTrace(context).ToLocalChecked());
      return Error{
          "Internal Error",
          0,
          0,
          std::string(*error_string, error_string.length()),
      };
    }
  }

  v8::Local<v8::Object> result = call_return_value.As<v8::Object>();

  context->SetAlignedPointerInEmbedderData(1, nullptr);

  auto success_field_name = v8::String::NewFromUtf8Literal(isolate_, "success");
  v8::Local<v8::Boolean> success = result->Get(context, success_field_name)
                                       .ToLocalChecked()
                                       .As<v8::Boolean>();
  assert(success->IsBoolean());

  if (!success->Value()) {
    auto error =
        result->Get(context, v8::String::NewFromUtf8Literal(isolate_, "error"))
            .ToLocalChecked()
            .As<v8::Object>();
    assert(error->IsObject());

    auto path =
        error->Get(context, v8::String::NewFromUtf8Literal(isolate_, "path"))
            .ToLocalChecked()
            .As<v8::String>();
    assert(path->IsString());
    auto line =
        error->Get(context, v8::String::NewFromUtf8Literal(isolate_, "line"))
            .ToLocalChecked()
            .As<v8::Number>();
    assert(line->IsNumber());
    auto column =
        error->Get(context, v8::String::NewFromUtf8Literal(isolate_, "column"))
            .ToLocalChecked()
            .As<v8::Number>();
    assert(column->IsNumber());
    auto message =
        error->Get(context, v8::String::NewFromUtf8Literal(isolate_, "message"))
            .ToLocalChecked()
            .As<v8::String>();
    assert(message->IsString());

    auto virtual_filepath = *VirtualFilesystem::AbsolutePath::FromString(
        ToStdString(isolate_, path));

    return Error{
        virtual_filesystem_->GetFile(virtual_filepath).diagnostics_path,
        static_cast<int>(line->Value()), static_cast<int>(column->Value()),
        ToStdString(isolate_, message)};
  }

  auto output_field_name = v8::String::NewFromUtf8Literal(isolate_, "output");
  v8::Local<v8::Object> output =
      result->Get(context, output_field_name).ToLocalChecked().As<v8::Object>();
  assert(output->IsObject());

  TranspileResults return_value;

  auto primary_module_field_name =
      v8::String::NewFromUtf8Literal(isolate_, "primary_module");
  auto primary_module = output->Get(context, primary_module_field_name)
                            .ToLocalChecked()
                            .As<v8::String>();
  return_value.primary_module = ToStdString(isolate_, primary_module);

  auto declaration_files_field_name =
      v8::String::NewFromUtf8Literal(isolate_, "declaration_files");
  v8::Local<v8::Object> declaration_files =
      output->Get(context, declaration_files_field_name)
          .ToLocalChecked()
          .As<v8::Object>();
  assert(declaration_files->IsObject());

  v8::Local<v8::Array> declaration_file_paths =
      declaration_files->GetOwnPropertyNames(context).ToLocalChecked();
  for (size_t i = 0; i < declaration_file_paths->Length(); ++i) {
    auto declaration_file_path = declaration_file_paths->Get(context, i)
                                     .ToLocalChecked()
                                     .As<v8::String>();
    std::string declaration_file_path_str =
        ToStdString(isolate_, declaration_file_path);
    auto declaration_file_contents =
        declaration_files->Get(context, declaration_file_path)
            .ToLocalChecked()
            .As<v8::String>();
    std::string declaration_file_conents_str =
        ToStdString(isolate_, declaration_file_contents);
    return_value.declaration_files.push_back(
        {declaration_file_path_str, declaration_file_conents_str});
  }

  auto js_modules_field_name =
      v8::String::NewFromUtf8Literal(isolate_, "js_modules");
  v8::Local<v8::Object> js_modules = output->Get(context, js_modules_field_name)
                                         .ToLocalChecked()
                                         .As<v8::Object>();
  assert(js_modules->IsObject());

  v8::Local<v8::Array> module_paths =
      js_modules->GetOwnPropertyNames(context).ToLocalChecked();
  for (size_t i = 0; i < module_paths->Length(); ++i) {
    auto module_path =
        module_paths->Get(context, i).ToLocalChecked().As<v8::String>();
    std::string module_path_str = ToStdString(isolate_, module_path);
    auto module_contents =
        js_modules->Get(context, module_path).ToLocalChecked().As<v8::String>();
    std::string module_conents_str = ToStdString(isolate_, module_contents);
    return_value.modules.push_back({module_path_str, module_conents_str});
  }

  if (!return_value.LookupPath(return_value.primary_module)) {
    return Error{diagnostics_path, 0, 0,
                 "Could not find the primary output module, '" +
                     return_value.primary_module +
                     "', in the emitted results."};
  }

  // Record the exported symbols from the module.
  auto primary_module_exports =
      output
          ->Get(context,
                v8::String::NewFromUtf8Literal(isolate_, "module_exports"))
          .ToLocalChecked()
          .As<v8::Array>();
  assert(primary_module_exports->IsArray());
  for (size_t i = 0; i < primary_module_exports->Length(); ++i) {
    auto module_export = primary_module_exports->Get(context, i)
                             .ToLocalChecked()
                             .As<v8::Object>();
    assert(module_export->IsObject());

    auto symbol_name = module_export
                           ->Get(context, v8::String::NewFromUtf8Literal(
                                              isolate_, "symbol_name"))
                           .ToLocalChecked()
                           .As<v8::String>();
    auto symbol_name_str = ToStdString(isolate_, symbol_name);

    auto type_string = module_export
                           ->Get(context, v8::String::NewFromUtf8Literal(
                                              isolate_, "type_string"))
                           .ToLocalChecked()
                           .As<v8::String>();
    auto type_string_str = ToStdString(isolate_, type_string);

    return_value.exported_symbols.push_back({symbol_name_str, type_string_str});
  }
  return return_value;
}

}  // namespace typescript_cpp_v8
}  // namespace reify
