#ifndef _REIFY_SRC_IDT_TARGETS_TYPESCRIPT_CPP_V8_CORE_GLOBAL_INITIALIZATION_H_
#define _REIFY_SRC_IDT_TARGETS_TYPESCRIPT_CPP_V8_CORE_GLOBAL_INITIALIZATION_H_

namespace reify {

class GlobalV8InitializationEnsurer {
 public:
  GlobalV8InitializationEnsurer();
  ~GlobalV8InitializationEnsurer();

  GlobalV8InitializationEnsurer(const GlobalV8InitializationEnsurer&) = delete;
  GlobalV8InitializationEnsurer(GlobalV8InitializationEnsurer&&) = delete;
};

}  // namespace reify

#endif  // _REIFY_SRC_IDT_TARGETS_TYPESCRIPT_CPP_V8_CORE_GLOBAL_INITIALIZATION_H_
