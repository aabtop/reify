cc_library(
    name = "interface",
    hdrs = [
        "include/reify/pure_cpp/object_visualizer.h",
        "include/reify/pure_cpp/scene_visualizer.h",
        "include/reify/pure_cpp/scene_visualizer_camera.h",
        "include/reify/pure_cpp/scene_visualizer_camera_2d.h",
        "include/reify/pure_cpp/scene_visualizer_camera_3d_arcball.h",
    ],
    srcs = [
        "scene_visualizer_camera_2d.cc",
        "scene_visualizer_camera_3d_arcball.cc",
    ],
    includes = [
        "include",
    ],
    visibility = ["//visibility:public"],
    deps = [
        "@vulkan_sdk//:vulkan",
        "@reify//src/window",
        "//src/utils",
        "@glm",
    ],
)
