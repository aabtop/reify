cc_library(
    name = "system_memory",
    visibility = ["//visibility:public"],
    deps = [":system_memory_header"] + select({
        "@bazel_tools//src/conditions:windows": [":system_memory_win32"],
        "//conditions:default": [":system_memory_linux"],
    }),
)

cc_library(
    name = "system_memory_header",
    hdrs = [
        "include/platform_specific/system_memory.h",
    ],
    includes = [
        "include",
    ],
)

cc_library(
    name = "system_memory_win32",
    srcs = ["src/windows/system_memory.cc"],
    tags = ["manual"],
    deps = [":system_memory_header"],
)

cc_library(
    name = "system_memory_linux",
    srcs = ["src/linux/system_memory.cc"],
    tags = ["manual"],
    deps = [":system_memory_header"],
)

cc_test(
    name = "test_system_memory",
    size = "small",
    srcs = [
        "src/system_memory_test.cc",
    ],
    deps = [
        ":system_memory",
        "@com_google_googletest//:gtest_main",
    ],
)
