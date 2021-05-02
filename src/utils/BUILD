load("@rules_cc//cc:defs.bzl", "cc_library", "cc_test")

cc_library(
    name = "utils",
    srcs = ["file_system.cc"],
    hdrs = [
        "include/reify/utils/file_system.h",
        "include/reify/utils/future.h",
    ],
    includes = [
        "include",
    ],
    deps = [],
)

cc_test(
    name = "utils_test",
    srcs = [
        "file_system_test.cc",
        "future_test.cc",
    ],
    deps = [
        ":utils",
        "@com_google_googletest//:gtest_main",
    ],
)
