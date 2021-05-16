#include "reify/typescript_cpp_v8.h"
#include "reify/utils/file_system.h"
#include "reify/utils/future.h"
#include "reify/utils/thread_with_work_queue.h"

namespace reify {
class Project::Impl {
 public:
  Impl(const std::vector<CompilerEnvironment::InputModule>* initial_modules,
       const std::filesystem::path& project_directory);

  reify::utils::Future<std::vector<std::shared_ptr<CompiledModule>>> Compile();

 private:
  const std::filesystem::path absolute_project_directory_;

  // We do all processing on a separate thread, since compilation can take
  // a non-trivial amount of time.
  reify::utils::ThreadWithWorkQueue thread_;

  reify::MountedHostFolderFilesystem virtual_filesystem_;
  std::optional<reify::CompilerEnvironment> compile_env_;
};

Project::Impl::Impl(
    const std::vector<CompilerEnvironment::InputModule>* initial_modules,
    const std::filesystem::path& project_directory)
    : absolute_project_directory_(std::filesystem::absolute(project_directory)),
      virtual_filesystem_(absolute_project_directory_) {
  thread_.Enqueue([this, initial_modules] {
    compile_env_.emplace(&virtual_filesystem_, initial_modules);
  });
}

reify::utils::Future<std::vector<std::shared_ptr<CompiledModule>>>
Project::Impl::Compile() {
  return thread_.Enqueue<std::vector<std::shared_ptr<CompiledModule>>>([this] {
    std::vector<std::filesystem::path> files_to_compile =
        reify::utils::FindMatchingFilenamesRecursively(
            absolute_project_directory_, std::regex("\\.ts$"));

    std::vector<std::shared_ptr<CompiledModule>> results;
    results.reserve(files_to_compile.size());

    for (const auto& source_file : files_to_compile) {
      auto maybe_virtual_path =
          virtual_filesystem_.HostPathToVirtualPath(source_file);
      if (!maybe_virtual_path) {
        continue;
      }

      auto maybe_compiled_module = compile_env_->Compile(*maybe_virtual_path);
      if (auto* error = std::get_if<0>(&maybe_compiled_module)) {
        continue;
      }

      results.push_back(std::get<1>(maybe_compiled_module));
    }

    return results;
  });
}

Project::Project(
    const std::vector<CompilerEnvironment::InputModule>* initial_modules,
    const std::filesystem::path& project_directory)
    : impl_(std::make_unique<Project::Impl>(initial_modules,
                                            project_directory)) {}

Project::~Project() {}

reify::utils::Future<std::vector<std::shared_ptr<CompiledModule>>>
Project::Compile() {
  return impl_->Compile();
}

}  // namespace reify
