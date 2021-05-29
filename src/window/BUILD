cc_library(
    name = "window",
    hdrs = [
        "include/reify/window/window.h",
    ],
    includes = [
        "include",
    ],
    visibility = ["//visibility:public"],
    deps = [
        "@vulkan_sdk//:vulkan",
    ],
)

cc_library(
    name = "window_utils",
    hdrs = [
        "include/reify/window/window_stack.h",
        "include/reify/window/window_viewport.h",
    ],
    srcs = [
        "window_stack.cc",
        "window_viewport.cc",
    ],
    includes = [
        "include",
    ],
    visibility = ["//visibility:public"],
    deps = [
        "@vulkan_sdk//:vulkan",
        "@reify//src/utils",
    ],
)

cc_library(
    name = "platform_window_wrapper",
    hdrs = [
        "include/reify/window/platform_window_wrapper.h",
    ],
    includes = [
        "include",
    ],
    srcs = [
      "platform_window_wrapper.cc",
    ],
    visibility = ["//visibility:public"],
    deps = [
        ":window",
        ":platform_window_renderer",
        "@platform_window",
        "@platform_window//:cpp",
        "@reify//src/vulkan_utils",
        "@reify//src/utils",
        "@fmtlib",
    ],
)

cc_library(
  name = "platform_window_renderer",
  srcs = ["platform_window_renderer.cc"],
  hdrs = ["include/reify/window/platform_window_renderer.h"],
  includes = ["include"],
  deps = [
    "@reify//src/vulkan_utils:swap_chain_renderer",
    "@vulkan_sdk//:vulkan",
    "@platform_window",
    "@platform_window//:vulkan",
  ],
  visibility = ["//visibility:public"],
)
