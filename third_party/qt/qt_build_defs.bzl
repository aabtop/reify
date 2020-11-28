# This is in its own file so that Qt build logic can be modified without
# modifying the `qt.BUILD` file itself, which would cause a re-fetch and rebuild
# of Qt, which sucks.

def _build_all_libs_impl(ctx):
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

def __qt_root_symlinks_impl(ctx):
  root_symlinks = {}
  for x in ctx.files.exe_sibling_files:
    root_symlinks[x.basename] = x

  return [DefaultInfo(runfiles=ctx.runfiles(symlinks=root_symlinks))]


_qt_root_symlinks = rule(
  implementation = __qt_root_symlinks_impl,
  attrs = {
    "exe_sibling_files": attr.label_list(mandatory = True, allow_files = True),
  },
)


QtInfo = provider(fields=["runtime_files", "root"])

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

  qt_components = [
    "Bodymovin",
    "Contacts",
    "Core",
    "DBus",
    "DocGallery",
    "Feedback",
    "Gui",
    "Network",
    "OpenGL",
    "Organizer",
    "Pdf",
    "PrintSupport",
    "PublishSubscribe",
    "Qml",
    "QmlModels",
    "QmlWorkerScript",
    "Quick",
    "QuickShapes",
    "QuickWidgets",
    "ServiceFramework",
    "Sql",
    "SystemInfo",
    "Versit",
    "VersitOrganizer",
    "WebChannel",
    "WebEngine",
    "WebEngineCore",
    "WebEngineWidgets",
    "WebSockets",
    "WebView",
    "Widgets",
  ]


  modules = [
    "libEGL",
    "libGLESv2",
  ] + ["Qt5{}".format(x) for x in qt_components]


  lib_filepaths = (
    ["lib/{}.lib".format(x) for x in modules]
  )

  include_directories = ["include"] + ["include/Qt{}".format(x) for x in qt_components]

  shared_library_filepaths = ["bin/{}.dll".format(x) for x in modules]

  resources = [
    "resources/icudtl.dat",
    "resources/qtwebengine_devtools_resources.pak",
    "resources/qtwebengine_resources.pak",
    "resources/qtwebengine_resources_100p.pak",
    "resources/qtwebengine_resources_200p.pak",
  ]

  plugins = [
    "plugins/platforms/qwindows.dll",
  ]

  translations = [
    "translations/qtwebengine_locales/en-US.pak",
  ]

  # Files which are expected to live as sibling files to the final executable.
  native.filegroup(
    name="qt_data_files",
    srcs = resources + plugins + translations,
    visibility = ["//visibility:public"],
  )

  native.filegroup(
    name="qt_data_sibling_files",
    srcs = ["bin/QtWebEngineProcess.exe"],
    visibility = ["//visibility:public"],
  )

  native.cc_library(
    name="qt_lib",
    srcs = lib_filepaths + shared_library_filepaths,
    hdrs = native.glob([x + "/**" for x in include_directories]),
    includes = include_directories,
    visibility = ["//visibility:public"],
  )

  native.exports_files(["bin/moc.exe", "bin/uic.exe"])
