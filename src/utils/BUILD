load("@rules_cc//cc:defs.bzl", "cc_library", "cc_test")

cc_library(
    name = "utils",
    srcs = [],
    hdrs = [
        "include/reify/utils/future.h",
    ],
    includes = [
        "include",
    ],
    deps = [],
)

cc_test(
    name = "utils_test",
    srcs = ["future_test.cc"],
    deps = [
        ":utils",
        "@com_google_googletest//:gtest_main",
    ],
)
