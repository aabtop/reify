cc_library(
    name = "interface",
    hdrs = ["include/reify/typescript_cpp_v8/domain_visualizer.h"],
    includes = [
        "include",
    ],
    visibility = ["//visibility:public"],
    deps = [
        "//src/idt/targets/typescript_cpp_v8:core",
        "@vulkan_sdk//:vulkan",
    ],
)

cc_library(
    name = "gui",
    srcs = [
        "domain_visualizer_gui.cc",
    ],
    hdrs = ["include/reify/typescript_cpp_v8/domain_visualizer_gui.h"],
    includes = [
        "include",
    ],
    visibility = ["//visibility:public"],
    deps = [
        ":interface",
        "//src/vulkan_utils",
        "@imgui//:demo",
        "@imgui//:frontend",
        "@imgui//:vulkan_backend",
    ],
)

cc_library(
    name = "visualizer_window",
    srcs = [
        "visualizer_window.cc",
    ],
    hdrs = [
        "include/reify/typescript_cpp_v8/visualizer_window.h",
    ],
    includes = [
        "include",
    ],
    visibility = ["//visibility:public"],
    deps = [
        "//src/idt/targets/typescript_cpp_v8/domain_visualizer:interface",
        "@platform_window",
        "@platform_window//:cpp",
        "@platform_window//:vulkan",
        "@reify//src/vulkan_utils",
        "@reify//src/vulkan_utils:window_renderer",
        "@reify//src/utils",
    ],
)
