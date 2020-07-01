#include <iostream>
#include <string_view>
#include <vector>

#include "compiled_module_impl.h"
#include "context_environment.h"
#include "generic_function_impl.h"
#include "global_initialization.h"
#include "public_include/reify.h"

namespace REIFY_GENERATED_PROJECT_NAMESPACE {
namespace reify {

namespace {

std::string ToStdString(v8::Isolate* isolate, const v8::Local<v8::Value> str) {
  v8::String::Utf8Value utf8_kind_value(isolate, str.template As<v8::String>());

  return std::string(*utf8_kind_value, utf8_kind_value.length());
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

  const auto context_environment = reinterpret_cast<ContextEnvironment*>(
      context->GetAlignedPointerFromEmbedderData(1));

  context_environment->source_file_import_stack.push_back(&module.path);
  bool failed =
      v8_module->InstantiateModule(context, &ResolveModuleCallback).IsNothing();
  context_environment->source_file_import_stack.pop_back();

  if (failed) {
    return v8::MaybeLocal<v8::Module>();
  }

  return v8_module;
}  // namespace

v8::MaybeLocal<v8::Module> ResolveModuleCallback(
    v8::Local<v8::Context> context, v8::Local<v8::String> specifier,
    v8::Local<v8::Module> referrer) {
  const auto context_environment = reinterpret_cast<ContextEnvironment*>(
      context->GetAlignedPointerFromEmbedderData(1));
  const auto transpile_results =
      context_environment->compiled_module->impl()->transpile_results();

  std::string specifier_as_str = ToStdString(context->GetIsolate(), specifier);

  const TypeScriptCompiler::Module* module;

  if (!specifier_as_str.empty() && specifier_as_str[0] == '.') {
    auto importer_directory =
        std::filesystem::path(
            *context_environment->source_file_import_stack.back())
            .parent_path();
    module = transpile_results.LookupPath(
        (importer_directory / (specifier_as_str + ".js")).lexically_normal());
  } else {
    module = transpile_results.LookupPath("/" + specifier_as_str + ".js");
  }

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
          ->compiled_module->impl()
          ->transpile_results();

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

// Returns a clone of the object, except the clone has a V8 internal field
// added to it which we can later use to cache an associated C++ object.
// Additionally, in order to ensure the C++ object (if created) stays in sync
// with the data in the struct, we take this opportunity to freeze the object
// as well.
void WithInternalField(const v8::FunctionCallbackInfo<v8::Value>& args) {
  v8::Isolate* isolate = args.GetIsolate();
  v8::HandleScope handle_scope(isolate);
  v8::Local<v8::Context> context = isolate->GetCurrentContext();

  if (args.Length() < 1 || !args[0]->IsObject()) {
    isolate->ThrowException(v8::String::NewFromUtf8(
        isolate, "Expected an object for the first argument."));
    return;
  }
  v8::Local<v8::Object> source_object = args[0].As<v8::Object>();
  if (source_object->InternalFieldCount() > 0) {
    // The source object already has an internal field, so do nothing and
    // just return it.
    args.GetReturnValue().Set(source_object);
    return;
  }

  auto blank_object_with_internal_field =
      reinterpret_cast<ContextEnvironment*>(
          context->GetAlignedPointerFromEmbedderData(1))
          ->blank_object_with_internal_field.Get(isolate);
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

  // Freeze all objects that might have an associated C++ object cached inside
  // of them.
  bool result =
      return_value->SetIntegrityLevel(context, v8::IntegrityLevel::kFrozen)
          .ToChecked();
  assert(result);

  args.GetReturnValue().Set(return_value);
}

void InstallRootFunctions(v8::Isolate* isolate,
                          v8::Local<v8::ObjectTemplate> global_template) {
  global_template->Set(isolate, "withInternalField",
                       v8::FunctionTemplate::New(isolate, WithInternalField));
}
}  // namespace

class RuntimeEnvironment::Impl {
 public:
  Impl(const std::shared_ptr<CompiledModule>& module);
  ~Impl();

  const std::optional<RuntimeException>& error() const { return error_; }

  std::variant<GetExportError, std::unique_ptr<GenericFunction::Impl>>
  GetGenericExport(std::string_view symbol_name);

 private:
  const std::shared_ptr<CompiledModule> module_;
  GlobalV8InitializationEnsurer global_v8_initialization_ensurer_;
  std::optional<RuntimeException> error_;
  v8::Isolate::CreateParams create_params_;
  v8::Isolate* isolate_ = nullptr;
  std::optional<ContextEnvironment> context_environment_;
  v8::Persistent<v8::Context> context_;
  v8::Persistent<v8::Object> primary_module_namespace_;
};

RuntimeEnvironment::Impl::Impl(const std::shared_ptr<CompiledModule>& module)
    : module_(module) {
  create_params_.array_buffer_allocator = nullptr;
  // Create a new Isolate and make it the current one.
  create_params_.array_buffer_allocator =
      v8::ArrayBuffer::Allocator::NewDefaultAllocator();
  isolate_ = v8::Isolate::New(create_params_);
  {
    v8::Isolate::Scope isolate_scope(isolate_);
    // Create a stack-allocated handle scope.
    v8::HandleScope handle_scope(isolate_);

    auto blank_object_with_internal_field = v8::ObjectTemplate::New(isolate_);
    blank_object_with_internal_field->SetInternalFieldCount(1);

    // Create a template for the global object that contains functions
    // from the generated interface.
    v8::Local<v8::ObjectTemplate> global_template =
        v8::ObjectTemplate::New(isolate_);
    InstallRootFunctions(isolate_, global_template);

    context_environment_ = ContextEnvironment{
        .compiled_module = module_,
        .blank_object_with_internal_field = v8::Persistent<v8::ObjectTemplate>(
            isolate_, blank_object_with_internal_field),
    };

    // Create a new context.
    v8::Local<v8::Context> context =
        v8::Context::New(isolate_, nullptr, global_template);
    context->SetAlignedPointerInEmbedderData(1, &(*context_environment_));

    context_.Reset(isolate_, context);

    // Enter the context for compiling and running the hello world script.
    v8::Context::Scope context_scope(context);

    // We're just about to compile the script; set up an error handler to
    // catch any exceptions the script might throw.
    v8::TryCatch try_catch(isolate_);

    auto evaluate_module_result = EvaluateModules(context);
    if (auto error = std::get_if<std::string>(&evaluate_module_result)) {
      error_ = "Error evaluating: " + *error;
      return;
    }

    v8::Local<v8::Module> primary_module =
        std::get<v8::Local<v8::Module>>(evaluate_module_result);

    v8::Local<v8::Object> primary_module_namespace =
        primary_module->GetModuleNamespace().As<v8::Object>();

    primary_module_namespace_.Reset(isolate_, primary_module_namespace);
  }
}

RuntimeEnvironment::Impl::~Impl() {
  if (!primary_module_namespace_.IsEmpty()) {
    primary_module_namespace_.Reset();
  }
  if (!context_.IsEmpty()) {
    context_.Reset();
  }
  context_environment_.reset();
  if (isolate_) {
    isolate_->LowMemoryNotification();
    isolate_->Dispose();
  }
  delete create_params_.array_buffer_allocator;
}

std::variant<GetExportError, std::unique_ptr<GenericFunction::Impl>>
RuntimeEnvironment::Impl::GetGenericExport(std::string_view symbol_name) {
  v8::Isolate::Scope isolate_scope(isolate_);
  v8::HandleScope handle_scope(isolate_);
  auto context = v8::Local<v8::Context>::New(isolate_, context_);
  v8::Context::Scope context_scope(context);
  v8::TryCatch try_catch(isolate_);
  v8::Local<v8::Object> primary_module_namespace =
      v8::Local<v8::Object>::New(isolate_, primary_module_namespace_);

  v8::Local<v8::Value> function_untyped_val;
  if (!primary_module_namespace
           ->Get(context, v8::String::NewFromUtf8(isolate_, symbol_name.data(),
                                                  v8::NewStringType::kNormal,
                                                  symbol_name.size())
                              .ToLocalChecked())
           .ToLocal(&function_untyped_val)) {
    return "Could not find function '" + std::string(symbol_name) + "'.";
  }
  if (!function_untyped_val->IsFunction()) {
    return "Symbol '" + std::string(symbol_name) + "' is not a function.";
  }

  v8::Local<v8::Function> entrypoint =
      v8::Local<v8::Function>::Cast(function_untyped_val);

  return std::make_unique<GenericFunction::Impl>(context, entrypoint);
}

std::variant<RuntimeException, RuntimeEnvironment> CreateRuntimeEnvironment(
    std::shared_ptr<CompiledModule> module) {
  auto impl = std::unique_ptr<RuntimeEnvironment::Impl>(
      new RuntimeEnvironment::Impl(module));
  if (impl->error()) {
    return *impl->error();
  }
  return RuntimeEnvironment(std::move(impl));
}

RuntimeEnvironment::RuntimeEnvironment(std::unique_ptr<Impl>&& impl)
    : impl_(std::move(impl)) {}
RuntimeEnvironment::~RuntimeEnvironment() {}

std::variant<GetExportError, GenericFunction>
RuntimeEnvironment::GetGenericExport(std::string_view symbol_name) {
  auto generic_function_impl_or_error = impl_->GetGenericExport(symbol_name);
  if (auto error =
          std::get_if<GetExportError>(&generic_function_impl_or_error)) {
    return *error;
  }
  return GenericFunction(
      std::move(std::get<std::unique_ptr<GenericFunction::Impl>>(
          generic_function_impl_or_error)));
}
}  // namespace reify
}  // namespace REIFY_GENERATED_PROJECT_NAMESPACE
