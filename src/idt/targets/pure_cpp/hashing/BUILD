load("@rules_cc//cc:defs.bzl", "cc_library", "cc_test")

cc_library(
    name = "hashing",
    srcs = ["src/cache.cc"],
    hdrs = [
        "public_include/reify/pure_cpp/cache.h",
        "public_include/reify/pure_cpp/hashing.h",
        "public_include/reify/pure_cpp/hashing_post_definitions.h",
    ],
    includes = [
        "public_include",
    ],
    visibility = ["//visibility:public"],
    # We use BLAKE3 for hashing:
    # https://github.com/BLAKE3-team/BLAKE3
    deps = [
        "@BLAKE3",
        "@ebb//:thread_pool",
    ],
)

cc_test(
    name = "test_cache",
    size = "small",
    srcs = [
        "src/cache_test.cc",
    ],
    deps = [
        ":hashing",
        "@com_google_googletest//:gtest_main",
    ],
)

cc_library(
    name = "thread_pool_cache_runner",
    srcs = [],
    hdrs = [
        "public_include/reify/pure_cpp/thread_pool_cache_runner.h",
    ],
    includes = [
        "public_include",
    ],
    visibility = ["//visibility:public"],
    deps = [
        ":hashing",
        "//src/platform_specific:system_memory",
        "@ebb//:thread_pool",
    ],
)
