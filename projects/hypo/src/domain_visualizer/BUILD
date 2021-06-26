cc_library(
    name = "domain_visualizer_hypo",
    srcs = [
        "domain_visualizer_hypo.cc",
        "free_camera_viewport_3d.cc",
        "free_camera_viewport_3d.h",
    ],
    hdrs = ["domain_visualizer_hypo.h"],
    deps = [
        "//:hypo-purecpp",
        "//:hypo-typescript_cpp_v8",
        "//:hypo_lib",
        "//src/domain_visualizer/vulkan:mesh_renderer",
        "@reify//src/idt/targets/typescript_cpp_v8/domain_visualizer:interface",
        "@imgui//:frontend",
    ],
    visibility = ["//visibility:public"],
)
