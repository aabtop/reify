load("@aabtop_rules_qt//:qt_rules.bzl", "qt_cc_library", "qt_cc_binary", "qt_resource")
load("@bazel_skylib//rules:copy_file.bzl", "copy_file")

copy_file(
  name = "index_html",
  src = "//src/monaco_wrapper:index.html",
  out = "index.html",
)
copy_file(
  name = "editor_worker_js",
  src = "//src/monaco_wrapper:editor.worker.js",
  out = "editor.worker.js",
)
copy_file(
  name = "ts_worker_js",
  src = "//src/monaco_wrapper:ts.worker.js",
  out = "ts.worker.js",
)
copy_file(
  name = "monaco_wrapper_js",
  src = "//src/monaco_wrapper:monaco_wrapper.js",
  out = "monaco_wrapper.js",
)
qt_resource(
  name = "monaco_wrapper_resources",
  srcs = ["index.html", "monaco_wrapper.js", "editor.worker.js", "ts.worker.js"],
)

qt_cc_library(
  name = "main_window",
  srcs = [
    "main_window.cc",
    "compilation.cc",
    "compilation.h",
  ],
  hdr = "main_window.h",
  ui_src = "main_window.ui",
  deps = [
    ":monaco_wrapper_resources",
    ":monaco_interface",
    ":about_dialog",
    ":domain_visualizer_qt_widget",
    "//src/idt/targets/typescript_cpp_v8/domain_visualizer:interface",
  ],
)

qt_cc_library(
  name = "monaco_interface",
  srcs = [
    "monaco_interface.cc",
  ],
  hdr = "monaco_interface.h",
  deps = [
    "//src/idt/targets/typescript_cpp_v8:core",
  ]
)

qt_cc_library(
  name = "about_dialog",
  srcs = [
    "about_dialog.cc",
  ],
  hdr = "about_dialog.h",
  ui_src = "about_dialog.ui",
)

# Embeds a DomainVisualizer into a Qt Widget for use in Qt applications.
qt_cc_library(
    name = "domain_visualizer_qt_widget",
    srcs = [
        "domain_visualizer_qt_widget.cc",
    ],
    hdr = "domain_visualizer_qt_widget.h",
    defines = select({
        "@bazel_tools//src/conditions:windows": ["BAZEL_TARGET_OS_WINDOWS"],
        "//conditions:default": ["BAZEL_TARGET_OS_LINUX"],
    }),
    deps = [
        "//src/window",
    ],
)

cc_library(
  name = "ide",
  srcs = [
    "ide.cc",
  ],
  hdrs = [
    "include/reify/typescript_cpp_v8/ide.h",
  ],
  includes = [
    "include",
  ],
  deps = [
    ":main_window",
    "//src/idt/targets/typescript_cpp_v8/domain_visualizer:interface",
  ],
  visibility = ["//visibility:public"],
)
