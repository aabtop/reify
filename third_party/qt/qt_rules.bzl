def _build_all_libs_impl(ctx):
#    if "windows" in ctx.attr.os:
#        output_filenames = ["v8_monolith.lib"]
#        build_script_env = ctx.configuration.default_shell_env
#        build_script_env.update({
#            "ProgramFiles(x86)": "C:\\Program Files (x86)",
#            "WINDIR": "C:\\Windows",
#        })
#        if ctx.attr.bazel_vc != "":
#            build_script_env.update({
#                "BAZEL_VC": ctx.attr.bazel_vc,
#            })
#    else:
#        output_filenames = ["libv8_monolith.a"]
#        build_script_env = {}
#    output_files = [ctx.actions.declare_file(x) for x in output_filenames]
#
#    ctx.actions.run(
#        outputs = output_files,
#        inputs = ctx.files.data,
#        tools = [ctx.executable.build_script],
#        arguments = [ctx.files.depot_tools_dir[0].path, ctx.files.v8_src_dir[0].path, ctx.attr.build_config] + [x.path for x in output_files],
#        progress_message = "Building V8",
#        executable = ctx.executable.build_script,
#        env = build_script_env,
#        execution_requirements = {"no-sandbox": "1", "local": "1"},
#    )
#
#    return [
#        DefaultInfo(
#            files = depset(output_files),
#        ),
#    ]
    output_filepaths = [
      "lib/Qt5Core.lib",
      "lib/Qt5Core.dll",
    ]

    include_directory = "include"

    output_files = [ctx.actions.declare_file(x) for x in output_filepaths] + [ctx.actions.declare_directory(include_directory)]

    return [
        DefaultInfo(
            files = depset(output_files),
        ),
    ]

build_all_libs = rule(
    implementation = _build_all_libs_impl,
    attrs = {
#        "build_script": attr.label(
#            mandatory = True,
#            allow_single_file = True,
#            executable = True,
#            cfg = "exec",
#        ),
#        "data": attr.label_list(
#            mandatory = True,
#        ),
#        "depot_tools_dir": attr.label(mandatory = True, allow_single_file = True),
        "src_dir": attr.label(mandatory = True, allow_single_file = True),
#        "build_config": attr.string(mandatory = True),
        "os": attr.string(mandatory = True),
        "bazel_vc": attr.string(),
    },
)

def define_qt_targets():
  build_all_libs(
    name="build_libs",
    src_dir=".",
    os = select({
        "@bazel_tools//src/conditions:windows": "windows",
        "//conditions:default": "linux",
    }),
    bazel_vc = "{bazel_vc}",
  )

  lib_filepaths = [
    "lib/Qt5Core.lib",
    "lib/Qt5Widgets.lib",
    "lib/Qt5Gui.lib",
    "lib/Qt5WebEngineWidgets.lib",
  ]

  include_directories = ["include", "include/QtWidgets", "include/QtWebEngineWidgets"]

  native.cc_library(
    name="qt",
    srcs = lib_filepaths,
    hdrs = native.glob([x + "/**" for x in include_directories]),
    includes = include_directories,
    visibility = ["//visibility:public"],
  )

  native.exports_files(["bin/moc.exe", "bin/uic.exe"])

def __qt_compile_ui_impl(ctx):
  ctx.actions.run(
    outputs = [ctx.outputs.out],
    inputs = ctx.files.ui_src,
    tools = [ctx.executable.uic],
    arguments = [ctx.files.ui_src[0].path, "-o", ctx.outputs.out.path],
    executable = ctx.executable.uic,
  )

  return DefaultInfo(files = depset([ctx.outputs.out]))


_qt_compile_ui = rule(
  implementation = __qt_compile_ui_impl,
  attrs = {
    "uic": attr.label(default = "@qt//:bin/uic.exe", allow_single_file = True, executable = True, cfg = "exec"),
    "ui_src": attr.label(mandatory = True, allow_single_file = True),
    "out": attr.output(mandatory = True),
  }
)


def qt_ui_header(name, ui_src):
  ui_header_file = "ui_{}.h".format(ui_src.split('.')[0])
  uic_header_target_name = "{}_uic".format(name)
  _qt_compile_ui(
      name = uic_header_target_name,
      ui_src = ui_src,
      out = ui_header_file,
  )

  native.cc_library(
      name = name,
      hdrs = [ui_header_file],
  )


def __qt_compile_moc_impl(ctx):
  ctx.actions.run(
    outputs = [ctx.outputs.out],
    inputs = ctx.files.hdr_src,
    tools = [ctx.executable.moc],
    arguments = [ctx.files.hdr_src[0].path, "-o", ctx.outputs.out.path, "-f\"{}\"".format(ctx.files.hdr_src[0].path)],
    executable = ctx.executable.moc,
  )

  return DefaultInfo(files = depset([ctx.outputs.out]))


_qt_compile_moc = rule(
  implementation = __qt_compile_moc_impl,
  attrs = {
    "moc": attr.label(default = "@qt//:bin/moc.exe", allow_single_file = True, executable = True, cfg = "exec"),
    "hdr_src": attr.label(mandatory = True, allow_single_file = True),
    "out": attr.output(mandatory = True),
    "package_name": attr.string(mandatory = True),
  }
)

def qt_moc_src(name, hdr_src):
  moc_src_file = "moc_{}.cc".format(hdr_src.split('.')[0])
  moc_target_name = "{}_moc".format(name)
  _qt_compile_moc(
      name = name,
      hdr_src = hdr_src,
      out = moc_src_file,
      package_name = native.package_name(),
  )


def qt_cc_library(name, srcs, hdr, ui_src, deps=None, **kwargs):
  moc_target_name = "{}_moc".format(name)
  qt_moc_src(moc_target_name, hdr)

  uic_target_name = "{}_ui".format(name)
  qt_ui_header(uic_target_name, ui_src)

  native.cc_library(
      name = name,
      srcs = srcs + [":" + moc_target_name],
      hdrs = [hdr],
      deps = deps + [":" + uic_target_name],
      **kwargs
  )
