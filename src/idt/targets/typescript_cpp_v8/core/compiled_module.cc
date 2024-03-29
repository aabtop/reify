#include "compiled_module_impl.h"
#include "public_include/reify/typescript_cpp_v8/typescript_cpp_v8.h"

namespace reify {
namespace typescript_cpp_v8 {

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

}  // namespace typescript_cpp_v8
}  // namespace reify
