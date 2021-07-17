cc_library(
    name = "object_visualizers",
    srcs = [
        "object_visualizer_region3.cc",
        "free_camera_viewport_3d.cc",
    ],
    hdrs = [
        "free_camera_viewport_3d.h",
        "object_visualizer_region3.h",
    ],
    deps = [
        "//:hypo-purecpp",
        "//:hypo_lib",
        "@reify//src/idt/targets/pure_cpp/object_visualizer:interface",
        "//src/visualizer/vulkan:mesh_renderer",
        "@imgui//:frontend",
        "@imgui-filebrowser",
    ],
    visibility = ["//visibility:public"],
)

cc_library(
    name = "typescript_symbol_visualizer",
    hdrs = ["typescript_symbol_visualizer.h"],
    deps = [
        ":object_visualizers",
        "@reify//src/idt/targets/typescript_cpp_v8/symbol_visualizer",
        "//:hypo-typescript_cpp_v8",
    ],
    visibility = ["//visibility:public"],
)
