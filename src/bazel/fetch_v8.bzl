load("@reify//:src/bazel/v8/fetch_depot_tools.bzl", "install_depot_tools", "prepend_depot_tools_args")


def __install_v8_repository_impl(repository_ctx):
  repository_ctx.report_progress("Downloading and extracting V8 source code")
  # This version is used by win-stable Chrome version 86.0.4240.75,
  # according to https://omahaproxy.appspot.com/.
  gclient_binary_path = repository_ctx.path(repository_ctx.attr.gclient_binary)

  repository_ctx.file(
    ".gclient",
    content="""
solutions = [
  { "name"        : 'v8',
    "url"         : 'https://github.com/v8/v8.git@8.6.395.10',
    "deps_file"   : 'DEPS',
    "managed"     : True,
    "custom_deps" : {
    },
    "custom_vars": {},
  },
]
    """,
  )

  # Run `gclient sync` to fetch all V8 external dependencies.
  repository_ctx.report_progress("Running `gclient sync`")
  #result = repository_ctx.execute(
  #  prepend_depot_tools_args(repository_ctx) + [gclient_binary_path, "sync", "--no-history", "--shallow", "--jobs", "16"],
  #  environment={
  #    "DEPOT_TOOLS_WIN_TOOLCHAIN": "0",
  #    "DEPOT_TOOLS_UPDATE": "0",
  #  },
  #  timeout=2000,
  #)
  #if result.return_code != 0:
  #  fail(result.stderr)

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
  install_depot_tools(
      name = "v8_depot_tools",
      build_file = "@reify//:src/bazel/v8/depot_tools.BUILD",
  )

#  _install_v8_repository(
#    name = "v8",
#    build_file = "@reify//:src/bazel/v8/v8.BUILD",
#    gclient_binary = "@v8_depot_tools//:gclient",
#  )
  native.new_local_repository(
    name = "v8",
    build_file = "@reify//:src/bazel/v8/v8.BUILD",
    path = "src/bazel/v8/local_v8",
  )

