#ifndef _HYPO_IDE_DOMAIN_VISUALIZER_HYPO_QT_H
#define _HYPO_IDE_DOMAIN_VISUALIZER_HYPO_QT_H

#include <QVulkanInstance>
#include <QWidget>
#include <memory>

#include "reify/typescript_cpp_v8/domain_visualizer.h"

class DomainVisualizerHypoQtVulkan
    : public reify::typescript_cpp_v8::DomainVisualizer {
 public:
  DomainVisualizerHypoQtVulkan(QWidget* frame);

  std::vector<reify::CompilerEnvironment::InputModule> GetTypeScriptModules()
      override;

  bool CanConsumeSymbol(
      const reify::CompiledModule::ExportedSymbol& symbol) override;

  void ConsumeSymbol(std::shared_ptr<reify::CompiledModule> module,
                     const reify::CompiledModule::ExportedSymbol& symbol,
                     const std::function<void(std::optional<ConsumeError>&&)>&
                         on_consumed) override;

 private:
  QVulkanInstance q_vulkan_instance_;
  std::unique_ptr<QWidget>
      vulkan_window_widget_;  // This is a unique_ptr to ensure it is destroyed
                              // before the QVulkanInstance object.

  std::unique_ptr<DomainVisualizer> wrapped_domain_visualizer_;
};

#endif  // _HYPO_IDE_DOMAIN_VISUALIZER_HYPO_QT_H
