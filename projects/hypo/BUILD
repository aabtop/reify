load("@reify//src/idt/targets/pure_cpp:pure_cpp.bzl", "idt_as_purecpp")
load("@reify//src/idt/targets/typescript:typescript.bzl", "idt_as_typescript")
load("@reify//src/idt:rules.bzl", "idt")
load("@reify//src/idt/targets/typescript_cpp_v8:typescript_cpp_v8.bzl", "idt_as_typescript_cpp_v8")

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
    ],
    includes = [
        "src/",
    ],
    deps = [
        ":hypo-purecpp",
        "@cgal",
    ]
)
cc_binary(
    name = "hypo",
    srcs = [
        "src/main.cc",
    ],
    includes = [
        "src/",
    ],
    linkstatic = 1,
    deps = [
        ":hypo_lib",
        ":hypo-purecpp",
        ":hypo-typescript_cpp_v8",
        "@cgal",
        "@reify//src/idt/targets/typescript_cpp_v8:command_line_tool",
        "@reify//third_party:CLI11",
    ],
)

alias(
    name = "ide",
    actual = "//src/ide:ide",
)