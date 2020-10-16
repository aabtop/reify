#v8_repository = repository_rule(
#    implementation=_v8_repository,
#    local=True,
#    attrs={"path": attr.string(mandatory=True)}
#)
load("@bazel_tools//tools/build_defs/repo:git.bzl", "new_git_repository")
load("@bazel_tools//tools/build_defs/repo:http.bzl", "http_archive")

def __install_v8_repository_impl(repository_ctx):
  repository_ctx.report_progress("Downloading and extracting V8 source code")
  # This version is used by win-stable Chrome version 86.0.4240.75,
  # according to https://omahaproxy.appspot.com/.
  repository_ctx.download_and_extract(
      url="https://github.com/v8/v8/archive/8.6.395.10.tar.gz",
      output=".",
      sha256="cad2a29dd1fb2ec0d4b7c266ae9de0ee0f1d2e0b4c8c5444ffe83e4e7ac76805",
      stripPrefix="v8-8.6.395.10",
  )

  prepend_args = []
  if "windows" in repository_ctx.os.name.lower():
    prepend_args = ["cmd", "/c"]
  gclient_binary_path = repository_ctx.path(repository_ctx.attr.gclient_binary)
  # Run `gclient config` because you have to before you call `gclient sync`.
  repository_ctx.report_progress("Running `gclient config`")
  result = repository_ctx.execute(
    prepend_args + [gclient_binary_path, "config", "https://github.com/v8/v8.git"],
    working_directory=".",
    environment={
      "DEPOT_TOOLS_WIN_TOOLCHAIN": "0",
      "DEPOT_TOOLS_UPDATE": "0",
    },
  )
  if result.return_code != 0:
    fail(result.stderr)

  # Run `gclient sync` to fetch all V8 external dependencies.
  repository_ctx.report_progress("Running `gclient sync`")
  result = repository_ctx.execute(
    prepend_args + [gclient_binary_path, "sync", "--no-history", "--shallow"],
    working_directory=".",
    environment={
      "DEPOT_TOOLS_WIN_TOOLCHAIN": "0",
      "DEPOT_TOOLS_UPDATE": "0",
    },
  )
  if result.return_code != 0:
    fail(result.stderr)

  # TODO if Linux run `./build/install-build-deps.sh`

  # Implant our V8 "BUILD" file into the repository.
  repository_ctx.symlink(repository_ctx.attr.build_file, "BUILD")


_install_v8_repository = repository_rule(
    implementation = __install_v8_repository_impl,
    attrs = {
      "build_file": attr.label(doc="Location of the v8.BUILD file.", mandatory=True,),
      "gclient_binary": attr.label(doc="Reference to the depot_tools files.", mandatory=True,),
    },
)

def install_v8_repository():
  new_git_repository(
      name = "v8_depot_tools",
      remote = "https://chromium.googlesource.com/chromium/tools/depot_tools.git",
      commit = "39d870e1f07f8908701d160b5445782cdd3f1541",
      build_file = "@reify//:src/bazel/v8/depot_tools.BUILD",
      shallow_since = "1602779265 +0000",
  )

  _install_v8_repository(
    name = "v8",
    build_file = "@reify//:src/bazel/v8/v8.BUILD",
    gclient_binary = "@v8_depot_tools//:gclient",
  )

