#ifndef REIFY_CONTEXT_ENVIRONMENT_H_
#define REIFY_CONTEXT_ENVIRONMENT_H_

#include <v8.h>

#include <string>
#include <vector>

#include "reify.h"

namespace REIFY_GENERATED_PROJECT_NAMESPACE {
namespace reify {

struct ContextEnvironment {
  std::shared_ptr<CompiledModule> compiled_module;
  v8::CopyablePersistentTraits<v8::ObjectTemplate>::CopyablePersistent
      blank_object_with_internal_field;

  // As we're loading the JavaScript files we maintain a stack of which source
  // files are currently being loaded and importing which other source files.
  std::vector<const std::string*> source_file_import_stack;
};

}  // namespace reify
}  // namespace REIFY_GENERATED_PROJECT_NAMESPACE

#endif  // REIFY_CONTEXT_ENVIRONMENT_H_