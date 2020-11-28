load("@reify//third_party/qt:qt_rules.bzl", "qt_cc_library", "qt_cc_binary")

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

qt_cc_library(
  name = "mainwindow",
  srcs = [
    "mainwindow.cc",
  ],
  hdr = "mainwindow.h",
  ui_src = "mainwindow.ui",
  qt_dep = "@qt//:qt",
)
