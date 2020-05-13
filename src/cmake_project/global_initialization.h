#ifndef _REIFY_GLOBAL_INITIALIZATION_H_
#define _REIFY_GLOBAL_INITIALIZATION_H_

#include "reify_generated_project_namespace.h"

namespace REIFY_GENERATED_PROJECT_NAMESPACE {
namespace reify {

class GlobalV8InitializationEnsurer {
 public:
  GlobalV8InitializationEnsurer();
  ~GlobalV8InitializationEnsurer();

  GlobalV8InitializationEnsurer(const GlobalV8InitializationEnsurer&) = delete;
  GlobalV8InitializationEnsurer(GlobalV8InitializationEnsurer&&) = delete;
};

}  // namespace reify
}  // namespace REIFY_GENERATED_PROJECT_NAMESPACE

#endif  // _REIFY_GLOBAL_INITIALIZATION_H_
