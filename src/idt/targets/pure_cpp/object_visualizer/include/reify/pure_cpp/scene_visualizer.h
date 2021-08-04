#ifndef _REIFY_PURE_CPP_SCENE_VISUALIZER_H_
#define _REIFY_PURE_CPP_SCENE_VISUALIZER_H_

#include "imgui.h"
#include "reify/pure_cpp/object_visualizer.h"
#include "reify/pure_cpp/scene_visualizer_camera.h"
#include "reify/utils/thread_with_work_queue.h"

namespace reify {
namespace pure_cpp {

template <typename ViewMatrix>
class SceneObjectRenderable {
 public:
  virtual ~SceneObjectRenderable() {}
  virtual utils::ErrorOr<window::Window::Renderer::FrameResources> Render(
      VkCommandBuffer command_buffer, VkFramebuffer framebuffer,
      VkImage output_color_image, const window::Rect& viewport_region,
      const ViewMatrix& view_projection_matrix) = 0;
};

template <typename ViewMatrix>
class SceneObject {
 public:
  virtual ~SceneObject() {}
  virtual utils::ErrorOr<std::unique_ptr<SceneObjectRenderable<ViewMatrix>>>
  CreateSceneObjectRenderable(VkInstance instance,
                              VkPhysicalDevice physical_device, VkDevice device,
                              VkFormat output_image_format) = 0;
  virtual reify::pure_cpp::ImGuiVisualizer* GetImGuiVisualizer() const = 0;
};

template <typename T, typename ViewMatrix>
class SceneVisualizer : public ObjectVisualizer<T>,
                        reify::window::Window,
                        reify::pure_cpp::ImGuiVisualizer {
 public:
  using CreateSceneObject = std::function<reify::utils::ErrorOr<
      std::shared_ptr<SceneObject<ViewMatrix>>>(const T& data)>;

  SceneVisualizer(SceneVisualizerCamera<ViewMatrix>* camera,
                  const CreateSceneObject& create_prepared_object)
      : camera_(camera), create_prepared_object_(create_prepared_object) {}

  utils::Future<utils::ErrorOr<std::any>> PrepareDataForPreview(
      const T& data) override;
  void SetPreview(const std::optional<std::any>& prepared_symbol) override;

  window::Window* GetWindow() const override {
    return const_cast<SceneVisualizer*>(this);
  }
  ImGuiVisualizer* GetImGuiVisualizer() const override {
    if (current_prepared_object_) {
      return const_cast<SceneVisualizer*>(this);
    } else {
      return nullptr;
    }
  };

  bool OnInputEvent(const InputEvent& input_event) override;

  void OnViewportResize(const std::array<int, 2>& size) override;

  void AdvanceTime(std::chrono::duration<float> seconds) override;

  reify::utils::ErrorOr<std::unique_ptr<Renderer>> CreateRenderer(
      VkInstance instance, VkPhysicalDevice physical_device, VkDevice device,
      VkFormat output_image_format) override;

  std::string ImGuiWindowPanelTitle() const override;
  void RenderImGuiWindow() override;

 private:
  class Renderer : public window::Window::Renderer {
   public:
    Renderer(VkInstance instance, VkPhysicalDevice physical_device,
             VkDevice device, VkFormat output_image_format,
             SceneVisualizer* parent)
        : instance_(instance),
          physical_device_(physical_device),
          device_(device),
          output_image_format_(output_image_format),
          parent_(parent) {}
    ~Renderer();

    utils::ErrorOr<FrameResources> RenderFrame(
        VkCommandBuffer command_buffer, VkFramebuffer framebuffer,
        VkImage output_color_image,
        const window::Rect& viewport_region) override;

   private:
    VkInstance instance_;
    VkPhysicalDevice physical_device_;
    VkDevice device_;
    VkFormat output_image_format_;

    SceneVisualizer* parent_;
  };

  SceneVisualizerCamera<ViewMatrix>* camera_;
  const CreateSceneObject create_prepared_object_;

  reify::utils::ThreadWithWorkQueue builder_thread_;

  std::shared_ptr<SceneObject<ViewMatrix>> current_prepared_object_;
  std::unique_ptr<SceneObjectRenderable<ViewMatrix>>
      current_renderable_prepared_object_;

  Renderer* renderer_ = nullptr;
};

template <typename T, typename ViewMatrix>
utils::Future<utils::ErrorOr<std::any>>
SceneVisualizer<T, ViewMatrix>::PrepareDataForPreview(const T& data) {
  return builder_thread_.EnqueueWithResult<reify::utils::ErrorOr<std::any>>(
      [data, create_prepared_object =
                 create_prepared_object_]() -> reify::utils::ErrorOr<std::any> {
        auto error_or_scene_object = create_prepared_object(data);
        if (auto error = std::get_if<0>(&error_or_scene_object)) {
          return *error;
        } else {
          return std::get<std::shared_ptr<SceneObject<ViewMatrix>>>(
              error_or_scene_object);
        }
      });
}

template <typename T, typename ViewMatrix>
void SceneVisualizer<T, ViewMatrix>::SetPreview(
    const std::optional<std::any>& prepared_symbol) {
  current_renderable_prepared_object_ = nullptr;
  if (!prepared_symbol) {
    current_prepared_object_ = nullptr;
  } else {
    current_prepared_object_ =
        std::any_cast<std::shared_ptr<SceneObject<ViewMatrix>>>(
            *prepared_symbol);
  }
}

template <typename T, typename ViewMatrix>
bool SceneVisualizer<T, ViewMatrix>::OnInputEvent(
    const InputEvent& input_event) {
  if (auto event = std::get_if<MouseMoveEvent>(&input_event)) {
    camera_->AccumulateMouseMove(event->x, event->y);
  } else if (auto event = std::get_if<MouseButtonEvent>(&input_event)) {
    camera_->AccumulateMouseButtonEvent(event->button, event->pressed, event->x,
                                        event->y);
  } else if (auto event = std::get_if<MouseWheelEvent>(&input_event)) {
    camera_->AccumulateMouseWheelEvent(event->angle_in_degrees, event->x,
                                       event->y);
  } else if (auto event = std::get_if<KeyboardEvent>(&input_event)) {
    camera_->AccumulateKeyboardEvent(event->key, event->pressed);
  }

  return false;
}

template <typename T, typename ViewMatrix>
void SceneVisualizer<T, ViewMatrix>::OnViewportResize(
    const std::array<int, 2>& size) {
  camera_->AccumulateViewportResize(size[0], size[1]);
}

template <typename T, typename ViewMatrix>
void SceneVisualizer<T, ViewMatrix>::AdvanceTime(
    std::chrono::duration<float> seconds) {
  camera_->AccumulateTimeDelta(seconds);
}

template <typename T, typename ViewMatrix>
std::string SceneVisualizer<T, ViewMatrix>::ImGuiWindowPanelTitle() const {
  if (current_prepared_object_->GetImGuiVisualizer()) {
    return current_prepared_object_->GetImGuiVisualizer()
        ->ImGuiWindowPanelTitle();
  } else {
    return "Scene Options";
  }
}

template <typename T, typename ViewMatrix>
void SceneVisualizer<T, ViewMatrix>::RenderImGuiWindow() {
  if (ImGui::Button("Reset Camera")) {
    camera_->Reset();
  }

  if (current_prepared_object_->GetImGuiVisualizer()) {
    current_prepared_object_->GetImGuiVisualizer()->RenderImGuiWindow();
  }
}

template <typename T, typename ViewMatrix>
reify::utils::ErrorOr<std::unique_ptr<window::Window::Renderer>>
SceneVisualizer<T, ViewMatrix>::CreateRenderer(VkInstance instance,
                                               VkPhysicalDevice physical_device,
                                               VkDevice device,
                                               VkFormat output_image_format) {
  return std::unique_ptr<window::Window::Renderer>(new Renderer(
      instance, physical_device, device, output_image_format, this));
}

template <typename T, typename ViewMatrix>
SceneVisualizer<T, ViewMatrix>::Renderer::~Renderer() {
  parent_->current_renderable_prepared_object_ = nullptr;
}

template <typename T, typename ViewMatrix>
utils::ErrorOr<window::Window::Renderer::FrameResources>
SceneVisualizer<T, ViewMatrix>::Renderer::RenderFrame(
    VkCommandBuffer command_buffer, VkFramebuffer framebuffer,
    VkImage output_color_image, const window::Rect& viewport_region) {
  if (!parent_->current_prepared_object_) {
    // Nothing to render, nothing to do.
    return FrameResources();
  }

  if (!parent_->current_renderable_prepared_object_) {
    // Lazily create the object we're about to render.
    REIFY_UTILS_ASSIGN_OR_RETURN(
        object_renderable,
        parent_->current_prepared_object_->CreateSceneObjectRenderable(
            instance_, physical_device_, device_, output_image_format_));
    parent_->current_renderable_prepared_object_ = std::move(object_renderable);
  }

  // Finally go ahead and execute the render.
  return parent_->current_renderable_prepared_object_->Render(
      command_buffer, framebuffer, output_color_image, viewport_region,
      parent_->camera_->ProjectionViewMatrix(viewport_region.width(),
                                             viewport_region.height()));
}

}  // namespace pure_cpp
}  // namespace reify

#endif  // _REIFY_PURE_CPP_SCENE_VISUALIZER_H_
