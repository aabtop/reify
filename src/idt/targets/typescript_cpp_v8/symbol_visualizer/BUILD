cc_library(
    name = "symbol_visualizer",
    srcs = ["symbol_visualizer.cc"],
    hdrs = ["include/reify/typescript_cpp_v8/symbol_visualizer.h"],
    includes = [
        "include",
    ],
    visibility = ["//visibility:public"],
    deps = [
        "//src/idt/targets/pure_cpp/object_visualizer:interface",
        "//src/idt/targets/typescript_cpp_v8:core",
        "@vulkan_sdk//:vulkan",
        "@reify//src/vulkan_utils",
        "@reify//src/window",
        "//src/utils",
        "@fmtlib",
    ],
)

cc_library(
    name = "imgui_widgets",
    srcs = [
        "imgui/widgets.cc",
    ],
    hdrs = ["include/reify/typescript_cpp_v8/imgui/widgets.h"],
    includes = [
        "include",
    ],
    visibility = ["//visibility:public"],
    deps = [
        "@imgui//:frontend",
        "@fmtlib",
    ],
)

cc_library(
    name = "imgui_utils",
    srcs = [],
    hdrs = ["include/reify/typescript_cpp_v8/imgui/utils.h"],
    includes = [
        "include",
    ],
    visibility = ["//visibility:public"],
    deps = [
        "@imgui//:frontend",
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
        ":imgui_widgets",
        ":imgui_utils",
        ":imgui_status_layer",
        "@imgui//:frontend",
        "@fmtlib",
        "@imgui-filebrowser",
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
        ":symbol_visualizer",
        ":imgui_widgets",
        ":imgui_status_layer",
        ":imgui_docking_layer",
        "//src/idt/targets/pure_cpp/hashing:thread_pool_cache_runner",
        "//src/platform_specific:system_memory",
        "@imgui//:frontend",
        "@fmtlib",
    ],
    visibility = ["//visibility:public"],
)

cc_library(
    name = "imgui_status_layer",
    srcs = [
        "imgui/status_layer.cc",
    ],
    hdrs = ["include/reify/typescript_cpp_v8/imgui/status_layer.h"],
    includes = [
        "include",
    ],
    deps = [
        ":imgui_docking_layer",
        ":imgui_utils",
        "@imgui//:frontend",
    ],
    visibility = ["//visibility:public"],
)

cc_library(
    name = "imgui_docking_layer",
    srcs = [
        "imgui/docking_layer.cc",
    ],
    hdrs = ["include/reify/typescript_cpp_v8/imgui/docking_layer.h"],
    includes = [
        "include",
    ],
    deps = [
        "@imgui//:frontend",
    ],
    visibility = ["//visibility:public"],
)

cc_library(
    name = "imgui_docking_freespace_to_window_viewport_layer",
    srcs = [
        "imgui/docking_freespace_to_window_viewport_layer.cc",
    ],
    hdrs = ["include/reify/typescript_cpp_v8/imgui/docking_freespace_to_window_viewport_layer.h"],
    includes = [
        "include",
    ],
    deps = [
        ":imgui_docking_layer",
        "//src/window:window_utils",
        "@imgui//:frontend",
    ],
    visibility = ["//visibility:public"],
)

cc_library(
    name = "imgui_common_stack",
    srcs = [
        "imgui/common_stack.cc",
    ],
    hdrs = ["include/reify/typescript_cpp_v8/imgui/common_stack.h"],
    includes = [
        "include",
    ],
    visibility = ["//visibility:public"],
    deps = [
        ":symbol_visualizer",
        ":imgui_layer_stack",
        ":imgui_project_layer",
        ":imgui_runtime_layer",
        ":imgui_status_layer",
        ":imgui_docking_layer",
        ":imgui_docking_freespace_to_window_viewport_layer",
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
        ":imgui_common_stack",
        ":imgui_layer_stack",
        ":imgui_project_layer",
        "//src/idt/targets/pure_cpp/hashing:thread_pool_cache_runner",
        "//src/window:window_utils",
        "//src/window:platform_window_wrapper",
        "//src/utils",
        "@CLI11",
        "@fmtlib",
    ],
)