cc_library(
    name = "BLAKE3",
    hdrs = [
      "c/blake3.h",
      "c/blake3_impl.h",
    ],
    srcs = [
      "c/blake3.c",
      "c/blake3_dispatch.c",
      "c/blake3_portable.c",
    ] + select({
        "@bazel_tools//src/conditions:windows": [
          "c/blake3_sse2_x86-64_windows_msvc.asm",
          "c/blake3_sse41_x86-64_windows_msvc.asm",
          "c/blake3_avx2_x86-64_windows_msvc.asm",
          "c/blake3_avx512_x86-64_windows_msvc.asm",
        ],
        "//conditions:default": [
          "c/blake3_sse2_x86-64_unix.S",
          "c/blake3_sse41_x86-64_unix.S",
          "c/blake3_avx2_x86-64_unix.S",
          "c/blake3_avx512_x86-64_unix.S",
        ],
    }),
    includes = [
        "c",
    ],
    visibility = ["//visibility:public"],
)

# Seems to only work on Linux because it #includes unistd.h.
cc_binary(
    name = "example",
    srcs = [
      "c/example.c",
    ],
    deps = [
      ":BLAKE3",
    ],
)
