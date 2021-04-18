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
  name = "mesh_renderer",
  srcs = ["mesh_renderer.cc"],
  hdrs = ["mesh_renderer.h", "triangle_soup.h"],
  deps = [
    "@vulkan_sdk//:vulkan",
    ":color_vert_as_header",
    ":color_frag_as_header",
    "@fmtlib",
    "@glm",
  ],
  visibility = ["//visibility:public"],
)
