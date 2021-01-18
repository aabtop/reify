#ifndef _IDE_DOMAIN_VISUALIZER_H
#define _IDE_DOMAIN_VISUALIZER_H

#include <future>
#include <optional>
#include <string>
#include <vector>

#include "reify/typescript_cpp_v8.h"

class DomainVisualizer {
 public:
  virtual std::vector<reify::CompilerEnvironment::InputModule>
  GetTypeScriptModules() = 0;

  virtual bool CanConsumeSymbol(
      const reify::CompiledModule::ExportedSymbol& symbol) = 0;

  using ConsumeError = std::string;
  virtual void ConsumeSymbol(
      std::shared_ptr<reify::CompiledModule> module,
      const reify::CompiledModule::ExportedSymbol& symbol,
      const std::function<void(std::optional<ConsumeError>&&)>&
          on_consumed) = 0;
};

#endif  // _IDE_DOMAIN_VISUALIZER_H
