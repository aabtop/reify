load("@reify//third_party/qt:qt_rules.bzl", "qt_cc_library")

cc_binary(
  name = "ide",
  srcs = [
    "main.cc",
  ],
  deps = [
    ":mainwindow",
    "@qt//:qt"
  ],
)

qt_cc_library(
  name = "mainwindow",
  srcs = [
    "mainwindow.cc",
  ],
  hdr = "mainwindow.h",
  ui_src = "mainwindow.ui",
  deps = ["@qt//:qt"],
)
