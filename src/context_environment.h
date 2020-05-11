#ifndef REIFY_CONTEXT_ENVIRONMENT_H_
#define REIFY_CONTEXT_ENVIRONMENT_H_

#include <v8.h>

#include "typescript_compiler.h"

struct ContextEnvironment {
  TypeScriptCompiler::TranspileResults* transpile_results;
  v8::Persistent<v8::ObjectTemplate> blank_object_with_internal_field;
};

#endif  // REIFY_CONTEXT_ENVIRONMENT_H_
