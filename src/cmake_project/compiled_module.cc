#include "compiled_module_impl.h"
#include "public_include/reify.h"

namespace REIFY_GENERATED_PROJECT_NAMESPACE {
namespace reify {

CompiledModule::CompiledModule(std::unique_ptr<Impl> impl)
    : impl_(std::move(impl)){};
CompiledModule::~CompiledModule() {}

const std::vector<CompiledModule::ExportedSymbol>&
CompiledModule::exported_symbols() const {
  return impl_->transpile_results().exported_symbols;
}

const CompiledModule::ExportedSymbol* CompiledModule::GetExportedSymbol(
    std::string_view export_symbol_name) const {
  for (const auto& exported_symbol : exported_symbols()) {
    if (export_symbol_name == exported_symbol.name) {
      return &exported_symbol;
    }
  }
  return nullptr;
}

}  // namespace reify
}  // namespace REIFY_GENERATED_PROJECT_NAMESPACE
