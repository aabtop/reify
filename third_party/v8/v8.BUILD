load("@reify//third_party/v8:v8_rules.bzl", "v8_library")

filegroup(
    name = "depot_tools_files",
    srcs = glob(["depot_tools/**"]),
)

filegroup(
    name = "v8_src_files",
    srcs = glob(["v8/v8/**"]),
)

config_setting(
    name = "dbg_mode",
    values = {
        "compilation_mode": "dbg",
    },
)

v8_library(
    name = "v8_lib_files",
    build_config = select({
        ":dbg_mode": "debug",
        "//conditions:default": "release",
    }),
    data = [
        ":depot_tools_files",
        ":v8_src_files",
    ],
    depot_tools_dir = "depot_tools",
    os = select({
        "@bazel_tools//src/conditions:windows": "windows",
        "//conditions:default": "linux",
    }),
    v8_src_dir = "v8/v8",
)

cc_library(
    name = "v8",
    srcs = [
        ":v8_lib_files",
    ],
    hdrs = glob(["v8/v8/include/**"]),
    includes = ["v8/v8/include"],
    linkstatic = 1,
    visibility = ["//visibility:public"],
)
