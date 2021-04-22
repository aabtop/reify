load("@rules_cc//cc:defs.bzl", "cc_library")

package(default_visibility = ["//visibility:public"])

cc_library(
    name = "frontend",
    srcs = [
        "imgui.cpp",
        "imgui_draw.cpp",
        "imgui_internal.h",
        "imgui_tables.cpp",
        "imgui_widgets.cpp",
        "imstb_rectpack.h",
        "imstb_textedit.h",
        "imstb_truetype.h",
    ],
    hdrs = [
        "imgui.h",
        "imconfig.h",
    ],
    includes = [
        ".",
    ],
)

cc_library(
    name = "vulkan_backend",
    srcs = [
        "backends/imgui_impl_vulkan.cpp",
    ],
    hdrs = [
        "backends/imgui_impl_vulkan.h",
    ],
    includes = [
        ".",
    ],
    deps = [
        ":frontend",
        "@vulkan_sdk//:vulkan",
    ],
)
