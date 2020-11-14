#ifndef _REIFY_GLOBAL_INITIALIZATION_H_
#define _REIFY_GLOBAL_INITIALIZATION_H_

namespace reify {

class GlobalV8InitializationEnsurer {
 public:
  GlobalV8InitializationEnsurer();
  ~GlobalV8InitializationEnsurer();

  GlobalV8InitializationEnsurer(const GlobalV8InitializationEnsurer&) = delete;
  GlobalV8InitializationEnsurer(GlobalV8InitializationEnsurer&&) = delete;
};

}  // namespace reify

#endif  // _REIFY_GLOBAL_INITIALIZATION_H_
