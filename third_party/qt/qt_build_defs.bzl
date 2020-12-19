# This is in its own file so that Qt build logic can be modified without
# modifying the `qt.BUILD` file itself, which would cause a re-fetch and rebuild
# of Qt, which sucks.

QtInfo = provider(fields=["runtime_files", "root"])

def define_qt_targets():
  qt_components = [
    "Bodymovin",
    "Core",
    "DBus",
    "Gui",
    "Network",
    "OpenGL",
    "Pdf",
    "PrintSupport",
    "Qml",
    "QmlModels",
    "QmlWorkerScript",
    "Quick",
    "QuickShapes",
    "QuickWidgets",
    "Sql",
    "WebChannel",
    "WebEngine",
    "WebEngineCore",
    "WebEngineWidgets",
    "WebSockets",
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
