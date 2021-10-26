cc_library(
    name = "object_visualizers",
    srcs = [
        "scene_object_boundary2.cc",
        "scene_object_region2.cc",
        "scene_object_region3.cc",
        "scene_object_trianglesoup3.cc",
    ],
    hdrs = [
        "scene_object_boundary2.h",
        "scene_object_region2.h",
        "scene_object_region3.h",
        "scene_object_trianglesoup3.h",
    ],
    visibility = ["//visibility:public"],
    deps = [
        "//:hypo-purecpp",
        "//:hypo_lib",
        "//src/visualizer/vulkan:renderers",
        "@imgui-filebrowser",
        "@imgui//:frontend",
        "@reify//src/idt/targets/pure_cpp/object_visualizer:interface",
    ],
)

cc_library(
    name = "typescript_symbol_visualizer",
    hdrs = ["typescript_symbol_visualizer.h"],
    visibility = ["//visibility:public"],
    deps = [
        ":object_visualizers",
        "//:hypo-typescript_cpp_v8",
        "@reify//src/idt/targets/typescript_cpp_v8/symbol_visualizer",
    ],
)
