load("@com_github_zaucy_rules_vulkan//:defs.bzl", "shader_binary")
load("@reify//third_party/qt:qt_rules.bzl", "qt_resource")
load("@reify//:src/bazel/utils.bzl", "convert_to_header")

shader_binary(
    name = "color_vert",
    srcs = ["color_vert.vert"],
)
shader_binary(
    name = "color_frag",
    srcs = ["color_frag.frag"],
)

convert_to_header("color_vert_as_header", ":color_vert", "color_vert.h")
convert_to_header("color_frag_as_header", ":color_frag", "color_frag.h")

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

cc_library(
  name = "renderer",
  srcs = ["renderer.cc"],
  hdrs = ["renderer.h"],
  deps = [
    "@vulkan_sdk//:vulkan",
    ":color_vert_as_header",
    ":color_frag_as_header",
    "@fmtlib",
    "@glm",
  ],
  visibility = ["//visibility:public"],
)
