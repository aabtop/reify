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
