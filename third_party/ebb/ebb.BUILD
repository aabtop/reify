cc_library(
    name = "thread_pool",
    srcs = [
        "src/fiber_condition_variable.cc",
        "src/thread_pool.cc",
    ],
    hdrs = [
        "src/fiber_condition_variable.h",
        "src/linked_list.h",
        "src/thread_pool.h",
    ],
    includes = [
        "src",
    ],
    visibility = ["//visibility:public"],
    deps = [
        ":platform",
    ],
)

cc_library(
    name = "platform",
    srcs = select({
        "@bazel_tools//src/conditions:windows": [
            "src/stdext/src/platform/win32/context.cc",
        ],
        "//conditions:default": [
            "src/stdext/src/platform/posix/context.cc",
        ],
    }),
    hdrs = [
        "src/stdext/src/platform/context.h",
    ],
    includes = [
        "src/stdext/src",
    ],
    visibility = ["//visibility:public"],
)

cc_library(
    name = "stdext",
    hdrs = [
        "src/stdext/src/stdext/align.h",
        "src/stdext/src/stdext/numeric.h",
        "src/stdext/src/stdext/optional.h",
    ],
    includes = [
        "src/stdext/src",
    ],
    visibility = ["//visibility:public"],
)
