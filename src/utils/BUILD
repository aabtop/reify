load("@rules_cc//cc:defs.bzl", "cc_library", "cc_test")

cc_library(
    name = "utils",
    srcs = [
        "file_system.cc",
        "thread_with_work_queue.cc",
    ],
    hdrs = [
        "include/reify/utils/file_system.h",
        "include/reify/utils/future.h",
        "include/reify/utils/thread_with_work_queue.h",
    ],
    includes = [
        "include",
    ],
    visibility = ["//visibility:public"],
    deps = [],
)

cc_test(
    name = "utils_test",
    srcs = [
        "file_system_test.cc",
        "future_test.cc",
        "thread_with_work_queue_test.cc",
    ],
    deps = [
        ":utils",
        "@com_google_googletest//:gtest_main",
    ],
)
