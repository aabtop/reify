#ifndef _IDE_DOMAIN_VISUALIZER_HYPO_H
#define _IDE_DOMAIN_VISUALIZER_HYPO_H

#include "src/ide/domain_visualizer.h"
#include "src/ide/vulkan/triangle_soup.h"

class DomainVisualizerHypo : public DomainVisualizer {
 public:
  DomainVisualizerHypo(const std::function<void(TriangleSoup&&)>& produce_mesh);

  std::vector<reify::CompilerEnvironment::InputModule> GetTypeScriptModules()
      override;

  bool CanConsumeSymbol(
      const reify::CompiledModule::ExportedSymbol& symbol) override;

  void ConsumeSymbol(std::shared_ptr<reify::CompiledModule> module,
                     const reify::CompiledModule::ExportedSymbol& symbol,
                     const std::function<void(std::optional<ConsumeError>&&)>&
                         on_consumed) override;

 private:
  std::function<void(TriangleSoup&&)> produce_mesh_;
};

#endif  // _IDE_DOMAIN_VISUALIZER_HYPO_H
