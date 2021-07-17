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
        "@platform_window//:platform_window_key",
        "@vulkan_sdk//:vulkan",
        "@reify//src/utils",
    ],
)

cc_library(
    name = "window_utils",
    srcs = [
        "window_stack.cc",
        "window_viewport.cc",
    ],
    hdrs = [
        "include/reify/window/window_stack.h",
        "include/reify/window/window_viewport.h",
    ],
    includes = [
        "include",
    ],
    visibility = ["//visibility:public"],
    deps = [
        ":window",
        "@reify//src/utils",
        "@reify//src/vulkan_utils",
        "@vulkan_sdk//:vulkan",
    ],
)

cc_library(
    name = "platform_window_wrapper",
    srcs = [
        "platform_window_wrapper.cc",
    ],
    hdrs = [
        "include/reify/window/platform_window_wrapper.h",
    ],
    includes = [
        "include",
    ],
    visibility = ["//visibility:public"],
    deps = [
        ":platform_window_renderer",
        ":window",
        "@fmtlib",
        "@platform_window",
        "@platform_window//:cpp",
        "@reify//src/utils",
        "@reify//src/vulkan_utils",
    ],
)

cc_library(
    name = "platform_window_renderer",
    srcs = ["platform_window_renderer.cc"],
    hdrs = ["include/reify/window/platform_window_renderer.h"],
    includes = ["include"],
    visibility = ["//visibility:public"],
    deps = [
        "@platform_window",
        "@platform_window//:vulkan",
        "@reify//src/vulkan_utils:swap_chain_renderer",
        "@vulkan_sdk//:vulkan",
    ],
)
