load("@reify//src/idt/targets/pure_cpp:pure_cpp.bzl", "idt_as_purecpp")
load("@reify//src/idt/targets/typescript:typescript.bzl", "idt_as_typescript")
load("@reify//src/idt:rules.bzl", "idt")
load("@reify//src/idt/targets/typescript_cpp_v8:typescript_cpp_v8.bzl", "idt_as_typescript_cpp_v8")
load("@aabtop_rules_qt//:qt_rules.bzl", "qt_cc_binary")

package(default_visibility = ["//visibility:public"])

idt(
    name = "hypo-idt",
    srcs = [
        "src/interface/ReifyInputInterface.hs",
    ],
    namespace = "hypo",
)

idt_as_purecpp(
    name = "hypo-purecpp",
    idt = ":hypo-idt",
    enable_hashes = True,
)

idt_as_typescript(
    name = "hypo-typescript",
    idt = ":hypo-idt",
    library_file = "src/interface/typescript/hypo.ts",
)

idt_as_typescript_cpp_v8(
    name = "hypo-typescript_cpp_v8",
    idt = ":hypo-idt",
    purecpp = ":hypo-purecpp",
    typescript = ":hypo-typescript",
)

cc_library(
    name = "hypo_lib",
    srcs = [
        "src/cgal/construct_region2.cc",
        "src/cgal/construct_region2.h",
        "src/cgal/construct_region3.cc",
        "src/cgal/construct_region3.h",
        "src/cgal/conversions.h",
        "src/cgal/embed_2d_in_3d.cc",
        "src/cgal/embed_2d_in_3d.h",
        "src/cgal/export_to_file.h",
        "src/cgal/export_to_stl.cc",
        "src/cgal/export_to_stl.h",
        "src/cgal/export_to_svg.cc",
        "src/cgal/export_to_svg.h",
        "src/cgal/extrude.cc",
        "src/cgal/extrude.h",
        "src/cgal/primitives_3d.cc",
        "src/cgal/primitives_3d.h",
        "src/cgal/subdivide.cc",
        "src/cgal/subdivide.h",
        "src/cgal/types_core.h",
        "src/cgal/types_nef_polyhedron_3.h",
        "src/cgal/types_polygons.h",
        "src/cgal/types_surface_mesh.h",
        "src/cgal/errors.h",
    ],
    includes = [
        "src/",
    ],
    deps = [
        ":hypo-purecpp",
        "@cgal",
        "@reify//src/idt/targets/pure_cpp/hashing:thread_pool_cache_runner",
    ]
)
cc_binary(
    name = "hypo",
    srcs = [
        "src/main_command_line.cc",
    ],
    linkstatic = 1,
    deps = [
        ":hypo_lib",
        ":hypo-purecpp",
        ":hypo-typescript_cpp_v8",
        "@cgal",
        "@reify//src/idt/targets/typescript_cpp_v8:command_line_tool",
        "@CLI11",
    ],
)

qt_cc_binary(
  name = "ide",
  srcs = [
    "src/main_ide.cc",
  ],
  deps = [
    "//src/visualizer:typescript_symbol_visualizer",
    "@reify//src/idt/targets/typescript_cpp_v8:ide",
    "@reify//src/idt/targets/typescript_cpp_v8/symbol_visualizer:imgui_layer_stack",
    "@reify//src/idt/targets/typescript_cpp_v8/symbol_visualizer:imgui_runtime_layer",
    "@reify//src/window:window",
    "@reify//src/window:window_utils",
  ],
)

cc_binary(
  name = "visualizer",
  srcs = [
    "src/main_visualizer.cc",
  ],
  deps = [
    "@reify//src/idt/targets/typescript_cpp_v8:visualizer_tool",
    "//src/visualizer:typescript_symbol_visualizer",
  ],
)
