#include "src/ide/domain_visualizer_hypo.h"

#include "reify/typescript_cpp_v8/hypo.h"

std::vector<reify::CompilerEnvironment::InputModule>
DomainVisualizerHypo::GetTypeScriptModules() {
  return reify::typescript_cpp_v8::hypo::typescript_declarations();
}

bool DomainVisualizerHypo::CanConsumeSymbol(
    const reify::CompiledModule::ExportedSymbol& symbol) {
  return (symbol.HasType<reify::Function<hypo::Region2()>>() ||
          symbol.HasType<reify::Function<hypo::Region3()>>());
}

void DomainVisualizerHypo::ConsumeSymbol(
    std::shared_ptr<reify::CompiledModule> module,
    const reify::CompiledModule::ExportedSymbol& symbol,
    const std::function<void(std::optional<ConsumeError>&&)>& on_consumed) {
  on_consumed("We did it!  symbol.name: " + symbol.name);
}
