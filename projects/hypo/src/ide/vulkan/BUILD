load("@com_github_zaucy_rules_vulkan//:defs.bzl", "shader_binary")
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

cc_library(
  name = "interface",
  srcs = [],
  hdrs = ["triangle_soup.h"],
  deps = [],
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
    ":interface",
    "@fmtlib",
    "@glm",
  ],
  visibility = ["//visibility:public"],
)
