#include "global_initialization.h"

#include <libplatform/libplatform.h>
#include <v8.h>

#include <cassert>
#include <memory>

namespace REIFY_GENERATED_PROJECT_NAMESPACE {
namespace reify {

namespace {
int g_global_v8_ref_count = 0;
std::unique_ptr<v8::Platform> g_platform = nullptr;

void InitializeV8() {
  g_platform = v8::platform::NewDefaultPlatform();
  v8::V8::InitializePlatform(g_platform.get());
  v8::V8::Initialize();
}

void DisposeV8() {
  v8::V8::Dispose();
  v8::V8::ShutdownPlatform();
  g_platform.reset();
}

void IncrementGlobalV8RefCount() {
  if (g_global_v8_ref_count++ == 0) {
    InitializeV8();
  }
}

void DecrementGlobalV8RefCount() {
  assert(g_global_v8_ref_count > 0);

  if (--g_global_v8_ref_count == 0) {
    DisposeV8();
  }
}

}  // namespace

GlobalV8InitializationEnsurer::GlobalV8InitializationEnsurer() {
  IncrementGlobalV8RefCount();
}

GlobalV8InitializationEnsurer::~GlobalV8InitializationEnsurer() {
  DecrementGlobalV8RefCount();
}

}  // namespace reify
}  // namespace REIFY_GENERATED_PROJECT_NAMESPACE
