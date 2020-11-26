#def _fetch_qt_impl(repository_ctx):
#    depot_tools_subdir = "depot_tools"
#    v8_subdir = "v8"
#
#    if "windows" in repository_ctx.os.name:
#        fetch_script = repository_ctx.path(repository_ctx.attr._fetch_windows)
#    else:
#        fetch_script = repository_ctx.path(repository_ctx.attr._fetch_linux)
#
#    result = repository_ctx.execute(
#        [fetch_script, depot_tools_subdir, v8_subdir, repository_ctx.attr.branch],
#        timeout=1500,
#    )
#    if result.return_code:
#        print(result.stdout)
#        print(result.stderr)
#        fail("Error fetching V8 repository")
#
#    if "BAZEL_VC" in repository_ctx.os.environ:
#        bazel_vc = repository_ctx.os.environ["BAZEL_VC"]
#    else:
#        bazel_vc = ""
#
#    repository_ctx.template(
#        "BUILD",
#        repository_ctx.attr._build_template,
#        substitutions={
#            "{depot_tools_subdir}": depot_tools_subdir,
#            "{v8_subdir}": v8_subdir,
#            "{bazel_vc}": bazel_vc,
#        },
#    )
#
#fetch_qt = repository_rule(
#    implementation = _fetch_v8_impl,
#    attrs = {
#        "branch": attr.string(mandatory = True, doc = "The tag or branch of the V8 repository to clone."),
#        "_fetch_linux": attr.label(default = "@reify//:third_party/v8/fetch_linux.sh"),
#        "_fetch_windows": attr.label(default = "@reify//:third_party/v8/fetch_windows.bat"),
#        "_build_template": attr.label(default = "@reify//third_party/v8:v8.BUILD"),
#    },
#)

def fetch_qt(name, branch):
  native.new_local_repository(
    name = name,
    path = "C:/Users/Andrew/src/qt/qt5-install",
    build_file = "@reify//third_party/qt:qt.BUILD")
