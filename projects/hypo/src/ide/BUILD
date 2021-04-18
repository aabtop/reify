load("@aabtop_rules_qt//:qt_rules.bzl", "qt_cc_library")
load("@bazel_skylib//rules:copy_file.bzl", "copy_file")

package(default_visibility = ["//visibility:public"])

exports_files(["main.cc"])

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
        "//src/ide/vulkan:mesh_renderer",
        "@reify//src/idt/targets/typescript_cpp_v8/ide:domain_visualizer_interface",
    ],
)
