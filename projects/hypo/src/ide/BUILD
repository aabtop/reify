load("@aabtop_rules_qt//:qt_rules.bzl", "qt_cc_library")
load("@bazel_skylib//rules:copy_file.bzl", "copy_file")

package(default_visibility = ["//visibility:public"])

exports_files(["main.cc"])

cc_library(
    name = "domain_visualizer_hypo",
    srcs = ["domain_visualizer_hypo.cc"],
    hdrs = ["domain_visualizer_hypo.h"],
    deps = [
        "//:hypo-purecpp",
        "//:hypo-typescript_cpp_v8",
        "//:hypo_lib",
        "//src/ide/vulkan:interface",
        "@reify//src/idt/targets/typescript_cpp_v8/ide:domain_visualizer_interface",
    ],
)

qt_cc_library(
    name = "domain_visualizer_hypo_qt_vulkan_window",
    srcs = [
        "domain_visualizer_hypo_qt_vulkan_window.cc",
        "free_camera_viewport_3d.cc",
        "free_camera_viewport_3d.h",
    ],
    hdr = "domain_visualizer_hypo_qt_vulkan_window.h",
    deps = [
        "//src/ide/vulkan:renderer",
        "@glm",
    ],
)

cc_library(
    name = "domain_visualizer_hypo_qt",
    srcs = ["domain_visualizer_hypo_qt.cc"],
    hdrs = ["domain_visualizer_hypo_qt.h"],
    defines = select({
        "@bazel_tools//src/conditions:windows": ["BAZEL_TARGET_OS_WINDOWS"],
        "//conditions:default": ["BAZEL_TARGET_OS_LINUX"],
    }),
    deps = [
        ":domain_visualizer_hypo",
        ":domain_visualizer_hypo_qt_vulkan_window",
        "@aabtop_qt_bin//:qt_lib",
    ],
)
