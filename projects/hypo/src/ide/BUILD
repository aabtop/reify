load("@reify//third_party/qt:qt_rules.bzl", "qt_cc_library", "qt_cc_binary", "qt_resource")
load("@bazel_skylib//rules:copy_file.bzl", "copy_file")

qt_cc_binary(
  name = "ide",
  srcs = [
    "main.cc",
  ],
  deps = [
    ":main_window",
  ],
  qt_dep = "@qt//:qt",
)

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

cc_library(
  name = "project",
  srcs = ["project.cc"],
  hdrs = [
    "project.h",
    "thread_checker.h"
  ],
  deps = ["@reify//src/idt/targets/typescript_cpp_v8:core"],
)

qt_cc_library(
  name = "main_window",
  srcs = [
    "main_window.cc",
  ],
  hdr = "main_window.h",
  ui_src = "main_window.ui",
  deps = [
    ":monaco_wrapper_resources",
    ":web_interface",
    ":about_dialog",
    ":domain_visualizer_interface",
    ":domain_visualizer_hypo_qt",
    ":project",
  ],
  qt_dep = "@qt//:qt",
)

cc_library(
  name = "domain_visualizer_interface",
  hdrs = ["domain_visualizer.h"],
)

cc_library(
  name = "domain_visualizer_interface_qt",
  hdrs = ["domain_visualizer_qt.h"],
  deps = [":domain_visualizer_interface", "@qt//:qt_lib"],
)

cc_library(
  name = "domain_visualizer_hypo",
  srcs = ["domain_visualizer_hypo.cc"],
  hdrs = ["domain_visualizer_hypo.h"],
  deps = [
    ":domain_visualizer_interface",
    "//:hypo-typescript_cpp_v8",
    "//:hypo-purecpp",
    "//:hypo_lib",
  ],
)

cc_library(
  name = "domain_visualizer_hypo_qt",
  srcs = ["domain_visualizer_hypo_qt.cc"],
  deps = [
    ":domain_visualizer_hypo",
    ":domain_visualizer_interface_qt",
    "//src/ide/vulkan:renderer",
  ],
)

qt_cc_library(
  name = "web_interface",
  srcs = [
    "web_interface.cc",
  ],
  hdr = "web_interface.h",
  qt_dep = "@qt//:qt",
)

qt_cc_library(
  name = "about_dialog",
  srcs = [
    "about_dialog.cc",
  ],
  hdr = "about_dialog.h",
  ui_src = "about_dialog.ui",
  qt_dep = "@qt//:qt",
)
