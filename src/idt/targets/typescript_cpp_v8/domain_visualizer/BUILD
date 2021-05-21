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
    name = "imgui_layer_stack",
    srcs = [
        "imgui/layer_stack.cc",
    ],
    hdrs = ["include/reify/typescript_cpp_v8/imgui/layer_stack.h"],
    includes = [
        "include",
    ],
    visibility = ["//visibility:public"],
    deps = [
        "//src/window",
        "//src/vulkan_utils",
        "@imgui//:frontend",
        "@imgui//:vulkan_backend",
    ],
)

cc_library(
    name = "imgui_project_layer",
    srcs = [
        "imgui/project_layer.cc",
    ],
    hdrs = ["include/reify/typescript_cpp_v8/imgui/project_layer.h"],
    includes = [
        "include",
    ],
    deps = [
        ":imgui_runtime_layer",
        "@imgui//:frontend",
    ],
    visibility = ["//visibility:public"],
)

cc_library(
    name = "imgui_runtime_layer",
    srcs = [
        "imgui/runtime_layer.cc",
    ],
    hdrs = ["include/reify/typescript_cpp_v8/imgui/runtime_layer.h"],
    includes = [
        "include",
    ],
    deps = [
        ":interface",
        "@imgui//:frontend",
    ],
    visibility = ["//visibility:public"],
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
        ":imgui_layer_stack",
        ":imgui_project_layer",
        ":imgui_runtime_layer",
        "//src/window:window_stack",
        "//src/window:platform_window_wrapper",
        "//src/utils",
        "//third_party:CLI11",
        "@fmtlib",
    ],
)