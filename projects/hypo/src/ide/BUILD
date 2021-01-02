load("@reify//third_party/qt:qt_rules.bzl", "qt_cc_library", "qt_cc_binary", "qt_resource")
load("@bazel_skylib//rules:copy_file.bzl", "copy_file")

qt_cc_binary(
  name = "ide",
  srcs = [
    "main.cc",
  ],
  deps = [
    ":mainwindow",
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

qt_cc_library(
  name = "mainwindow",
  srcs = [
    "mainwindow.cc",
  ],
  hdr = "mainwindow.h",
  ui_src = "mainwindow.ui",
  deps = [":monaco_wrapper_resources", ":web_interface", ":about_dialog"],
  qt_dep = "@qt//:qt",
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
