cc_library(
    name = "object_visualizers",
    srcs = [
        "scene_object_boundary2.cc",
        "scene_object_mesh3.cc",
        "scene_object_region2.cc",
        "scene_object_region3.cc",
        "scene_object_trianglesoup3.cc",
        "construct_visualizer_svg.cc",
        "scene_object_svg_elements.cc",
    ],
    hdrs = [
        "scene_object_boundary2.h",
        "scene_object_mesh3.h",
        "scene_object_region2.h",
        "scene_object_region3.h",
        "scene_object_trianglesoup3.h",
        "visualizer_svg_types.h",
        "construct_visualizer_svg.h",
        "scene_object_svg_elements.h",
    ],
    visibility = ["//visibility:public"],
    deps = [
        "//:hypo-purecpp",
        "//:hypo_cgal_lib",
        "//:hypo_svg_lib",
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
