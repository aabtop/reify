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
        "@reify//src/window",
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
    name = "visualizer_tool",
    srcs = [
        "visualizer_tool.cc",
    ],
    hdrs = ["include/reify/typescript_cpp_v8/visualizer_tool.h"],
    includes = [
        "include",
    ],

    visibility = ["//visibility:public"],
    deps = [
        ":interface",
        ":gui",
        "//src/window:platform_window_wrapper",
    ],
)