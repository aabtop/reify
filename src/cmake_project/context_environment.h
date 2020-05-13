#ifndef REIFY_CONTEXT_ENVIRONMENT_H_
#define REIFY_CONTEXT_ENVIRONMENT_H_

#include <v8.h>

#include "reify.h"

namespace REIFY_GENERATED_PROJECT_NAMESPACE {
namespace reify {

struct ContextEnvironment {
  std::shared_ptr<CompiledModule> compiled_module;
  v8::CopyablePersistentTraits<v8::ObjectTemplate>::CopyablePersistent
      blank_object_with_internal_field;
};

}  // namespace reify
}  // namespace REIFY_GENERATED_PROJECT_NAMESPACE

#endif  // REIFY_CONTEXT_ENVIRONMENT_H_
