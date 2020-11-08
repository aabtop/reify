load("@reify//third_party/v8:v8_rules.bzl", "v8_library")

filegroup(
    name = "depot_tools_files",
    srcs = glob(["{depot_tools_subdir}/**"]),
)

filegroup(
    name = "v8_src_files",
    srcs = glob(["{v8_subdir}/v8/**"], exclude = ["v8/v8/tools/swarming_client/example/**"]),
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
    build_script = select({
        "@bazel_tools//src/conditions:windows": "@reify//third_party/v8:build_windows.bat",
        "//conditions:default": "@reify//third_party/v8:build_linux.sh",
    }),
    data = [
        ":depot_tools_files",
        ":v8_src_files",
    ],
    depot_tools_dir = "{depot_tools_subdir}",
    os = select({
        "@bazel_tools//src/conditions:windows": "windows",
        "//conditions:default": "linux",
    }),
    v8_src_dir = "{v8_subdir}/v8",
    bazel_vc = "{bazel_vc}",
)

cc_library(
    name = "v8",
    srcs = [
        ":v8_lib_files",
    ],
    hdrs = glob(["{v8_subdir}/v8/include/**"]),
    includes = ["{v8_subdir}/v8/include"],
    linkstatic = 1,
    visibility = ["//visibility:public"],
    linkopts = select({
        "@bazel_tools//src/conditions:windows": [
            "/DEFAULTLIB:dbghelp.lib",
            "/DEFAULTLIB:winmm.lib"
        ],
        "//conditions:default": [],
    }),
    defines = [
      "V8_COMPRESS_POINTERS",
    ] + select({
          "@bazel_tools//src/conditions:windows": ["_ITERATOR_DEBUG_LEVEL=0",],
          "//conditions:default": [],
    }),
)
