#include "compiled_module_impl.h"
#include "public_include/reify.h"

namespace REIFY_GENERATED_PROJECT_NAMESPACE {
namespace reify {

CompiledModule::CompiledModule(std::unique_ptr<Impl> impl)
    : impl_(std::move(impl)){};
CompiledModule::~CompiledModule() {}

}  // namespace reify
}  // namespace REIFY_GENERATED_PROJECT_NAMESPACE
