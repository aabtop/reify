load("@com_github_zaucy_rules_vulkan//:defs.bzl", "shader_binary")
load("@reify//:src/bazel/utils.bzl", "convert_to_header")

cc_library(
  name = "vulkan_utils",
  srcs = ["vulkan_utils.cc"],
  hdrs = ["include/vulkan_utils/vulkan_utils.h"],
  includes = ["include"],
  deps = [
    "@vulkan_sdk//:vulkan",
    "@fmtlib",
    "@glm",
  ],
  visibility = ["//visibility:public"],
)

cc_library(
  name = "swap_chain_renderer",
  srcs = ["swap_chain_renderer.cc"],
  hdrs = ["include/vulkan_utils/swap_chain_renderer.h"],
  includes = ["include"],
  deps = [
    ":vulkan_utils",
    "@vulkan_sdk//:vulkan",
  ],
  visibility = ["//visibility:public"],
)

cc_library(
  name = "window_renderer",
  srcs = ["window_renderer.cc"],
  hdrs = ["include/vulkan_utils/window_renderer.h"],
  includes = ["include"],
  deps = [
    ":swap_chain_renderer",
    "@vulkan_sdk//:vulkan",
    "@platform_window",
    "@platform_window//:vulkan",
  ],
  visibility = ["//visibility:public"],
)
