#include <QCoreApplication>
#include <QVBoxLayout>
#include <filesystem>
#include <iostream>

#include "src/ide/domain_visualizer_hypo.h"
#include "src/ide/domain_visualizer_hypo_qt_vulkan_window.h"
#include "src/ide/domain_visualizer_qt.h"

namespace {

#if defined(BAZEL_TARGET_OS) && BAZEL_TARGET_OS == LINUX

// Returns an absolute path to the current executable.
std::filesystem::path GetAbsoluteExecutableDir() {
  return std::filesystem::absolute(
      QCoreApplication::applicationDirPath().toStdString());
}

std::optional<std::filesystem::path> FindSoLibDir() {
  std::optional<std::filesystem::path> previous_path;
  for (std::filesystem::path current_path = GetAbsoluteExecutableDir();
       current_path.has_parent_path() &&
       (!previous_path || *previous_path != current_path);
       current_path = current_path.parent_path()) {
    previous_path = current_path;
    for (const auto& p : std::filesystem::directory_iterator(current_path)) {
      if (!std::filesystem::is_directory(p.path())) {
        continue;
      }

      auto path_as_string = p.path().filename().string();
      if (path_as_string.rfind("_solib", 0) == 0) {
        return p.path();
      }
    }
  }
  return std::nullopt;
}

// Walks up the directory tree back to the root looking for vulkan.so.1 file,
// so that we can use the one that's packaged with this application.
std::optional<std::filesystem::path> FindVulkanSo1() {
  auto so_lib_dir = FindSoLibDir();
  if (!so_lib_dir) {
    return std::nullopt;
  }

  for (const auto& p :
       std::filesystem::recursive_directory_iterator(*so_lib_dir)) {
    if (!std::filesystem::is_regular_file(p.path())) {
      continue;
    }

    if (p.path().filename() == "libvulkan.so.1") {
      return p.path();
    }
  }

  return std::nullopt;
}

#else  // #if defined(BAZEL_TARGET_OS) && BAZEL_TARGET_OS == LINUX

std::optional<std::filesystem::path> FindVulkanSo1() { return std::nullopt; }

#endif  // #if defined(BAZEL_TARGET_OS) && BAZEL_TARGET_OS == LINUX

}  // namespace

class DomainVisualizerQtVulkan : public DomainVisualizer {
 public:
  DomainVisualizerQtVulkan(QWidget* frame) {
    auto maybe_vulkan_so_1_path = FindVulkanSo1();
    if (maybe_vulkan_so_1_path) {
      // Qt doesn't know where our custom built Vulkan loader is on Linux, so
      // we have to find it and specify it manually.
      qputenv("QT_VULKAN_LIB", QByteArray(reinterpret_cast<const char*>(
                                   maybe_vulkan_so_1_path->c_str())));
    } else {
      std::cerr << "Could not find local build of vulkan.so.1. "
                << "Maybe the system version will work..." << std::endl;
    }
    q_vulkan_instance_.setLayers(QByteArrayList()
                                 << "VK_LAYER_LUNARG_standard_validation");
    if (!q_vulkan_instance_.create())
      qFatal("Failed to create Vulkan instance: %d",
             q_vulkan_instance_.errorCode());

    auto vulkan_window = new DomainVisualizerVulkanWindow();
    vulkan_window->setVulkanInstance(&q_vulkan_instance_);

    vulkan_window_widget_.reset(
        QWidget::createWindowContainer(vulkan_window, frame));

    QVBoxLayout* layout = new QVBoxLayout();
    layout->addWidget(vulkan_window_widget_.get());
    frame->setLayout(layout);

    wrapped_domain_visualizer_.reset(
        new DomainVisualizerHypo([vulkan_window](TriangleSoup&& x) {
          emit vulkan_window->SetTriangleSoup(
              std::make_shared<const TriangleSoup>(std::move(x)));
        }));
  }

  std::vector<reify::CompilerEnvironment::InputModule> GetTypeScriptModules()
      override {
    return wrapped_domain_visualizer_->GetTypeScriptModules();
  }

  bool CanConsumeSymbol(
      const reify::CompiledModule::ExportedSymbol& symbol) override {
    return wrapped_domain_visualizer_->CanConsumeSymbol(symbol);
  }

  void ConsumeSymbol(std::shared_ptr<reify::CompiledModule> module,
                     const reify::CompiledModule::ExportedSymbol& symbol,
                     const std::function<void(std::optional<ConsumeError>&&)>&
                         on_consumed) override {
    return wrapped_domain_visualizer_->ConsumeSymbol(module, symbol,
                                                     std::move(on_consumed));
  }

 private:
  QVulkanInstance q_vulkan_instance_;
  std::unique_ptr<QWidget>
      vulkan_window_widget_;  // This is a unique_ptr to ensure it is destroyed
                              // before the QVulkanInstance object.

  std::unique_ptr<DomainVisualizer> wrapped_domain_visualizer_;
};

std::unique_ptr<DomainVisualizer> CreateDefaultQtWidgetDomainVisualizer(
    QWidget* frame) {
  return std::make_unique<DomainVisualizerQtVulkan>(frame);
}
