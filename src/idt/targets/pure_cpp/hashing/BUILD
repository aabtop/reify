cc_library(
    name = "hashing",
    srcs = [],
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
    deps = ["@BLAKE3"],
)
