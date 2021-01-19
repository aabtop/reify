#include "src/ide/domain_visualizer_hypo.h"

#include "reify/typescript_cpp_v8.h"
#include "reify/typescript_cpp_v8/hypo.h"

DomainVisualizerHypo::DomainVisualizerHypo() {}

std::vector<reify::CompilerEnvironment::InputModule>
DomainVisualizerHypo::GetTypeScriptModules() {
  return reify::typescript_cpp_v8::hypo::typescript_declarations();
}

bool DomainVisualizerHypo::CanConsumeSymbol(
    const reify::CompiledModule::ExportedSymbol& symbol) {
  return (/*symbol.HasType<reify::Function<hypo::Region2()>>() ||*/
          symbol.HasType<reify::Function<hypo::Region3()>>());
}

void DomainVisualizerHypo::ConsumeSymbol(
    std::shared_ptr<reify::CompiledModule> module,
    const reify::CompiledModule::ExportedSymbol& symbol,
    const std::function<void(std::optional<ConsumeError>&&)>& on_consumed) {
  std::thread thread([module, symbol = std::move(symbol),
                      on_consumed = std::move(on_consumed)]() {
    // Setup a V8 runtime environment around the CompiledModule.  This will
    // enable us to call exported functions and query exported values from the
    // module.
    auto runtime_env_or_error = reify::CreateRuntimeEnvironment(module);
    if (auto error = std::get_if<0>(&runtime_env_or_error)) {
      on_consumed(*error);
      return;
    }

    reify::RuntimeEnvironment runtime_env(
        std::move(std::get<1>(runtime_env_or_error)));

    if (symbol.HasType<reify::Function<hypo::Region3()>>()) {
      auto entry_point_or_error =
          runtime_env.GetExport<reify::Function<hypo::Region3()>>(symbol.name);
      if (auto error = std::get_if<0>(&entry_point_or_error)) {
        on_consumed("Problem finding entrypoint function: " + *error);
        return;
      }
      auto entry_point = &std::get<1>(entry_point_or_error);

      auto result_or_error = entry_point->Call();
      if (auto error = std::get_if<0>(&result_or_error)) {
        on_consumed("Error running function: " + *error);
        return;
      }

      auto result = std::get<1>(result_or_error);
      on_consumed(std::nullopt);
    }
  });

  thread.detach();  // TODO: Manage the threading framework better.
}
