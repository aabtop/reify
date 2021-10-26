load("@com_github_zaucy_rules_vulkan//:defs.bzl", "shader_binary")
load("@reify//:src/bazel/utils.bzl", "convert_to_header")

shader_binary(
    name = "region3_flat_shaded_vert",
    srcs = ["region3_flat_shaded_vert.vert"],
)

shader_binary(
    name = "region2_solid_fill_vert",
    srcs = ["region2_solid_fill_vert.vert"],
)

shader_binary(
    name = "color_frag",
    srcs = ["color_frag.frag"],
)

convert_to_header("region3_flat_shaded_vert_as_header", ":region3_flat_shaded_vert", "region3_flat_shaded_vert.h")

convert_to_header("region2_solid_fill_vert_as_header", ":region2_solid_fill_vert", "region2_solid_fill_vert.h")

convert_to_header("color_frag_as_header", ":color_frag", "color_frag.h")

cc_library(
    name = "renderers",
    srcs = [
        "flat_shaded_triangle_renderer3.cc",
        "simple_simplex_renderer2.cc",
    ],
    hdrs = [
        "flat_shaded_triangle_renderer3.h",
        "simple_simplex_renderer2.h",
    ],
    visibility = ["//visibility:public"],
    deps = [
        ":color_frag_as_header",
        ":region2_solid_fill_vert_as_header",
        ":region3_flat_shaded_vert_as_header",
        "//src/geometry",
        "@glm",
        "@reify//src/vulkan_utils",
        "@reify//src/window",
        "@vulkan_sdk//:vulkan",
    ],
)
