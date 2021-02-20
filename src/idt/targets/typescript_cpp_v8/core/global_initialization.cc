#include "global_initialization.h"

#include <libplatform/libplatform.h>
#include <v8.h>

#include <cassert>
#include <memory>
#include <mutex>
#include <optional>

namespace reify {

namespace {

class V8Instance {
 public:
  V8Instance() {
    platform_ = v8::platform::NewDefaultPlatform();
    v8::V8::InitializePlatform(platform_.get());
    v8::V8::Initialize();
  }

  ~V8Instance() {
    v8::V8::Dispose();
    v8::V8::ShutdownPlatform();
    platform_.reset();
  }

 private:
  std::unique_ptr<v8::Platform> platform_;
};

struct V8State {
  std::mutex mutex;
  int global_v8_ref_count = 0;
  std::optional<V8Instance> instance;
};

V8State* GetV8State() {
  static V8State v8_state;
  return &v8_state;
}

void IncrementGlobalV8RefCount() {
  auto v8_state = GetV8State();
  std::lock_guard<std::mutex> lock(v8_state->mutex);

  if (v8_state->global_v8_ref_count++ == 0) {
    if (!v8_state->instance) {
      v8_state->instance.emplace();
    }
  }
}

void DecrementGlobalV8RefCount() {
  auto v8_state = GetV8State();
  std::lock_guard<std::mutex> lock(v8_state->mutex);

  // Unfortunately, I'm experiencing crashes that I'm having a hard time
  // debugging when we dispose of V8, and then later re-initialize it.
  // So, we'll leave this out and just shutdown when the process ends.
  // It's suggested by
  // https://v8-users.narkive.com/EgxKClHl/how-to-reinitialize-v8-after-v8-v8-dispose
  // that you shouldn't try to re-initialize V8 after disposing of it.
  if (--v8_state->global_v8_ref_count == 0) {
    // v8_state->instance = std::nullopt;
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
