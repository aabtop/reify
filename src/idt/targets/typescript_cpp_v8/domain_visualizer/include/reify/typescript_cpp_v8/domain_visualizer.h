#ifndef _IDT_TARGETS_TYPESCRIPT_CPP_V8_DOMAIN_VISUALIZER_DOMAIN_VISUALIZER_H
#define _IDT_TARGETS_TYPESCRIPT_CPP_V8_DOMAIN_VISUALIZER_DOMAIN_VISUALIZER_H

#include <vulkan/vulkan.h>

#include <any>
#include <array>
#include <functional>
#include <optional>
#include <string>
#include <vector>

#include "reify/typescript_cpp_v8.h"
#include "reify/window/window.h"

namespace reify {
namespace typescript_cpp_v8 {

class DomainVisualizer : public window::Window {
 public:
  template <typename T>
  using ErrorOr = window::Window::ErrorOr<T>;
  using PreparedSymbol = std::any;

  virtual ~DomainVisualizer(){};

  virtual std::vector<reify::CompilerEnvironment::InputModule>
  GetTypeScriptModules() = 0;

  virtual bool CanPreviewSymbol(
      const reify::CompiledModule::ExportedSymbol& symbol) = 0;

  // The callback, `on_preview_prepared`, may be called from a separate thread.
  virtual void PrepareSymbolForPreview(
      std::shared_ptr<reify::CompiledModule> module,
      const reify::CompiledModule::ExportedSymbol& symbol,
      const std::function<void(ErrorOr<PreparedSymbol>)>&
          on_preview_prepared) = 0;
  virtual void Preview(const PreparedSymbol& prepared_symbol) = 0;
};

}  // namespace typescript_cpp_v8
}  // namespace reify

#endif  // _IDT_TARGETS_TYPESCRIPT_CPP_V8_DOMAIN_VISUALIZER_DOMAIN_VISUALIZER_H
