#ifndef _IDE_DOMAIN_VISUALIZER_HYPO_H
#define _IDE_DOMAIN_VISUALIZER_HYPO_H

#include <any>

#include "cgal/types_nef_polyhedron_3.h"
#include "reify/purecpp/hypo.h"
#include "reify/typescript_cpp_v8/domain_visualizer.h"
#include "reify/utils/future.h"
#include "reify/utils/thread_with_work_queue.h"
#include "src/domain_visualizer/free_camera_viewport_3d.h"
#include "src/domain_visualizer/vulkan/mesh_renderer.h"
#include "src/domain_visualizer/vulkan/triangle_soup.h"

namespace ImGui {
// We forward declare this class to avoid including `imfilebrowser.h`, which
// includes <windows.h>, which is huge.
class FileBrowser;
}  // namespace ImGui

class ImGuiVisualizer {
 public:
  virtual std::string ImGuiWindowPanelTitle() const = 0;
  virtual void RenderImGuiWindow() = 0;
};

template <typename T>
class ObjectVisualizer {
 public:
  virtual reify::utils::Future<
      reify::typescript_cpp_v8::DomainVisualizer::ErrorOr<std::any>>
  PrepareDataForPreview(const T& data) = 0;
  virtual void SetPreview(const std::optional<std::any>& prepared_symbol) = 0;

  virtual reify::window::Window* GetWindow() const = 0;
  virtual ImGuiVisualizer* GetImGuiVisualizer() const { return nullptr; }
};

struct TypeScriptSymbolVisualizer {
  using PrepareSymbolForPreviewFunction = std::function<reify::utils::Future<
      reify::typescript_cpp_v8::DomainVisualizer::ErrorOr<std::any>>(
      reify::typescript_cpp_v8::RuntimeEnvironment*,
      const std::shared_ptr<reify::typescript_cpp_v8::CompiledModule>&,
      const reify::typescript_cpp_v8::CompiledModule::ExportedSymbol&)>;
  using SetPreviewFunction =
      std::function<void(const std::optional<std::any>&)>;

  std::string typescript_type;

  reify::window::Window* window;
  ImGuiVisualizer* im_gui_visualizer;

  PrepareSymbolForPreviewFunction prepare_symbol_for_preview_function;
  SetPreviewFunction set_preview_function;
};

template <typename T>
std::optional<TypeScriptSymbolVisualizer> MakeTypeScriptSymbolVisualizer(
    ObjectVisualizer<T>* domain_visualizer) {
  return TypeScriptSymbolVisualizer{
      reify_v8::TypeScriptTypeString<T>::value(),
      domain_visualizer->GetWindow(), domain_visualizer->GetImGuiVisualizer(),
      [domain_visualizer](
          reify::typescript_cpp_v8::RuntimeEnvironment* runtime_env,
          const std::shared_ptr<reify::typescript_cpp_v8::CompiledModule>&
              module,
          const reify::typescript_cpp_v8::CompiledModule::ExportedSymbol&
              symbol)
          -> reify::utils::Future<
              reify::typescript_cpp_v8::DomainVisualizer::ErrorOr<std::any>> {
        auto entry_point_or_error =
            runtime_env->GetExport<reify::typescript_cpp_v8::Function<T()>>(
                symbol.name);
        if (auto error = std::get_if<0>(&entry_point_or_error)) {
          reify::utils::Promise<
              reify::typescript_cpp_v8::DomainVisualizer::ErrorOr<std::any>>
              error_promise;
          error_promise.set(reify::typescript_cpp_v8::DomainVisualizer::Error{
              "Problem finding entrypoint function: " + *error});
          return error_promise.future();
        }
        auto entry_point = &std::get<1>(entry_point_or_error);

        auto result_or_error = entry_point->Call();
        if (auto error = std::get_if<0>(&result_or_error)) {
          reify::utils::Promise<
              reify::typescript_cpp_v8::DomainVisualizer::ErrorOr<std::any>>
              error_promise;
          error_promise.set(reify::typescript_cpp_v8::DomainVisualizer::Error{
              "Error running function: " + *error});
          return error_promise.future();
        }

        return domain_visualizer->PrepareDataForPreview(
            std::get<1>(result_or_error));
      },
      [domain_visualizer](const std::optional<std::any>& symbol) {
        domain_visualizer->SetPreview(symbol);
      }};
}

class TypeScriptSymbolVisualizerStack
    : public reify::typescript_cpp_v8::DomainVisualizer {
 public:
  TypeScriptSymbolVisualizerStack(
      const std::vector<
          reify::typescript_cpp_v8::CompilerEnvironment::InputModule>&
          typescript_modules,
      const std::vector<TypeScriptSymbolVisualizer>& visualizers);

  std::vector<reify::typescript_cpp_v8::CompilerEnvironment::InputModule>
  GetTypeScriptModules() override {
    return typescript_modules_;
  }

  bool CanPreviewSymbol(
      const reify::typescript_cpp_v8::CompiledModule::ExportedSymbol& symbol)
      override;

  reify::utils::Future<
      reify::typescript_cpp_v8::DomainVisualizer::ErrorOr<std::any>>
  PrepareSymbolForPreview(
      std::shared_ptr<reify::typescript_cpp_v8::CompiledModule> module,
      const reify::typescript_cpp_v8::CompiledModule::ExportedSymbol& symbol)
      override;
  void SetPreview(const std::any& prepared_symbol) override;
  void ClearPreview() override;

  bool OnInputEvent(const InputEvent& input_event) override;

  void OnViewportResize(const std::array<int, 2>& size) override;

  void AdvanceTime(std::chrono::duration<float> seconds) override;

  reify::typescript_cpp_v8::DomainVisualizer::ErrorOr<std::unique_ptr<Renderer>>
  CreateRenderer(VkInstance instance, VkPhysicalDevice physical_device,
                 VkDevice device, VkFormat output_image_format) override;

  bool HasImGuiWindow() const override;
  std::string ImGuiWindowPanelTitle() const override;
  void RenderImGuiWindow() override;

 private:
  class Renderer;

  struct PreparedSymbol {
    std::any processed_data;
    const TypeScriptSymbolVisualizer* associated_visualizer;
  };

  const TypeScriptSymbolVisualizer* FindVisualizerForSymbol(
      const reify::typescript_cpp_v8::CompiledModule::ExportedSymbol& symbol)
      const;

  const std::vector<reify::typescript_cpp_v8::CompilerEnvironment::InputModule>
      typescript_modules_;
  const std::unordered_map<std::string, TypeScriptSymbolVisualizer>
      typescript_type_to_visualizer_;

  reify::utils::ThreadWithWorkQueue runtime_thread_;

  std::optional<PreparedSymbol> selected_symbol_;

  std::optional<std::array<int, 2>> last_viewport_resize_;

  Renderer* renderer_;
};

class ObjectVisualizerHypoRegion3 : public ObjectVisualizer<hypo::Region3>,
                                    public reify::window::Window,
                                    public ImGuiVisualizer {
 public:
  ObjectVisualizerHypoRegion3();
  ~ObjectVisualizerHypoRegion3();

  reify::utils::Future<
      reify::typescript_cpp_v8::DomainVisualizer::ErrorOr<std::any>>
  PrepareDataForPreview(const hypo::Region3& data) override;
  void SetPreview(const std::optional<std::any>& prepared_symbol) override;

  reify::window::Window* GetWindow() const override {
    return const_cast<ObjectVisualizerHypoRegion3*>(this);
  }
  ImGuiVisualizer* GetImGuiVisualizer() const override {
    return const_cast<ObjectVisualizerHypoRegion3*>(this);
  }

  bool OnInputEvent(const InputEvent& input_event) override;

  void OnViewportResize(const std::array<int, 2>& size) override;

  void AdvanceTime(std::chrono::duration<float> seconds) override;

  reify::typescript_cpp_v8::DomainVisualizer::ErrorOr<std::unique_ptr<Renderer>>
  CreateRenderer(VkInstance instance, VkPhysicalDevice physical_device,
                 VkDevice device, VkFormat output_image_format) override;

  std::string ImGuiWindowPanelTitle() const override;
  void RenderImGuiWindow() override;

 private:
  reify::utils::ThreadWithWorkQueue builder_thread_;

  std::shared_ptr<TriangleSoup> pending_triangle_soup_;
  FreeCameraViewport3d free_camera_viewport_;
  MeshRenderer* mesh_renderer_ = nullptr;

  std::shared_ptr<hypo::cgal::Nef_polyhedron_3> current_preview_;

  std::unique_ptr<ImGui::FileBrowser> export_file_selector_;
};

class HypoTypeScriptSymbolVisualizerStack {
 public:
  HypoTypeScriptSymbolVisualizerStack();

  ObjectVisualizerHypoRegion3 region_3_visualizer;
  TypeScriptSymbolVisualizerStack visualizer;
};

#endif  // _IDE_DOMAIN_VISUALIZER_HYPO_H
