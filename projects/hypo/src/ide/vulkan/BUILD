load("@com_github_zaucy_rules_vulkan//:defs.bzl", "shader_binary")
load("@reify//third_party/qt:qt_rules.bzl", "qt_resource")

shader_binary(
    name = "color_vert",
    srcs = ["color_vert.vert"],
)
shader_binary(
    name = "color_frag",
    srcs = ["color_frag.frag"],
)
qt_resource(
  name = "triangle_renderer_resources",
  srcs = [":color_vert", ":color_frag"],
)

cc_library(
  name = "triangle_renderer",
  srcs = ["triangle_renderer.cc"],
  hdrs = ["triangle_renderer.h"],
  deps = [
    ":triangle_renderer_resources",
    "@qt//:qt_lib",
  ],
  visibility = ["//visibility:public"],
)
