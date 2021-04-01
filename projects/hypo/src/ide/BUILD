load("@aabtop_rules_qt//:qt_rules.bzl", "qt_cc_library", "qt_cc_binary", "qt_resource")
load("@bazel_skylib//rules:copy_file.bzl", "copy_file")

package(default_visibility = ["//visibility:public"])

exports_files(["main.cc"])

copy_file(
  name = "index_html",
  src = "@reify//src/monaco_wrapper:index.html",
  out = "index.html",
)
copy_file(
  name = "editor_worker_js",
  src = "@reify//src/monaco_wrapper:editor.worker.js",
  out = "editor.worker.js",
)
copy_file(
  name = "ts_worker_js",
  src = "@reify//src/monaco_wrapper:ts.worker.js",
  out = "ts.worker.js",
)
copy_file(
  name = "monaco_wrapper_js",
  src = "@reify//src/monaco_wrapper:monaco_wrapper.js",
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
    ":domain_visualizer_interface",
    ":domain_visualizer_hypo_qt",
  ],
)

cc_library(
  name = "domain_visualizer_interface",
  hdrs = ["domain_visualizer.h"],
)

cc_library(
  name = "domain_visualizer_interface_qt",
  hdrs = ["domain_visualizer_qt.h"],
  deps = [":domain_visualizer_interface", "@aabtop_qt_bin//:qt_lib"],
)

cc_library(
  name = "domain_visualizer_hypo",
  srcs = ["domain_visualizer_hypo.cc"],
  hdrs = ["domain_visualizer_hypo.h"],
  deps = [
    ":domain_visualizer_interface",
    "//src/ide/vulkan:interface",
    "//:hypo-typescript_cpp_v8",
    "//:hypo-purecpp",
    "//:hypo_lib",
  ],
)

qt_cc_library(
  name = "domain_visualizer_hypo_qt_vulkan_window",
  srcs = ["domain_visualizer_hypo_qt_vulkan_window.cc"],
  hdr = "domain_visualizer_hypo_qt_vulkan_window.h",
  deps = [
    "//src/ide/vulkan:renderer",
  ],
)

cc_library(
  name = "domain_visualizer_hypo_qt",
  srcs = ["domain_visualizer_hypo_qt.cc"],
  deps = [
    ":domain_visualizer_hypo",
    ":domain_visualizer_interface_qt",
    ":domain_visualizer_hypo_qt_vulkan_window",
  ],
  defines = select({
            "@bazel_tools//src/conditions:windows": ["BAZEL_TARGET_OS_WINDOWS"],
            "//conditions:default": ["BAZEL_TARGET_OS_LINUX"],
        }),
)


qt_cc_library(
  name = "monaco_interface",
  srcs = [
    "monaco_interface.cc",
  ],
  hdr = "monaco_interface.h",
  deps = ["//:hypo-typescript_cpp_v8",],
)

qt_cc_library(
  name = "about_dialog",
  srcs = [
    "about_dialog.cc",
  ],
  hdr = "about_dialog.h",
  ui_src = "about_dialog.ui",
)
