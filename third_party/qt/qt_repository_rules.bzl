load("@bazel_tools//tools/build_defs/repo:git.bzl", "git_repository")

def _fetch_qt_impl(repository_ctx):
    tmp_dir = "tmp"
    src_dir = tmp_dir + "/src"
    build_dir = tmp_dir + "/build"
  
    repository_ctx.download_and_extract(
      "http://download.qt.io/official_releases/qt/5.15/5.15.2/single/qt-everywhere-src-5.15.2.zip",
      stripPrefix="qt-everywhere-src-5.15.2/",
      sha256="6c5d37aa96f937eb59fd4e9ce5ec97f45fbf2b5de138b086bdeff782ec661733",
      output=src_dir,
    )

    env = {}
    if "windows" in repository_ctx.os.name:
      build_script = repository_ctx.path(repository_ctx.attr._build_script_windows)
      if "BAZEL_VC" in repository_ctx.os.environ:
        env["BAZEL_VC"] = repository_ctx.os.environ["BAZEL_VC"]
        #env["ProgramFiles(x86)"] = "C:\\Program Files (x86)"
        #env["WINDIR"] = "C:\\Windows"
    else:
      build_script = repository_ctx.path(repository_ctx.attr._build_script_linux)

    repository_ctx.report_progress("Building Qt...")

    result = repository_ctx.execute(
      [build_script, src_dir, build_dir, "."],
      timeout=43200,  # This can take a long time...
      environment = env,
    )
    if result.return_code:
      fail("Error building Qt.")

    # We clean up the checked out source code and the intermediate build files
    # so that the result that we actually cache is as small as possible.
    repository_ctx.report_progress("Deleting scratch files...")
    result = repository_ctx.delete(tmp_dir)
    if not result:
      fail("Error deleting scratch folder.")

    repository_ctx.report_progress("Creating Qt BUILD file...")

    repository_ctx.template(
        "BUILD",
        repository_ctx.attr._build_template,
        substitutions={
            "{src_dir}": src_dir,
        },
    )

fetch_qt = repository_rule(
  implementation = _fetch_qt_impl,
  attrs = {
    "_build_script_windows": attr.label(default = "@reify//third_party/qt:build_windows.bat"),
    "_build_script_linux": attr.label(default = "@reify//third_party/qt:build_linux.bat"),
    "_build_template": attr.label(default = "@reify//third_party/qt:qt.BUILD"),
  }
)
