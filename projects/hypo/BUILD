load("@reify//src/third_party/idt:rules.bzl", "idt", "idt_as_purecpp")

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

cc_library(
    name = "CLI11",
    hdrs = glob(["src/third_party/CLI11/include/**/*.h",
                 "src/third_party/CLI11/include/**/*.hpp"]),
    includes = [
        "src/third_party/CLI11/include",
    ],
)

cc_library(
    name = "hypo",
    srcs = [
        "src/main.cc",
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
        "src/cgal/types_surface_mesh.h",
        "src/cgal/types_polygons.h",
    ],
    includes = [
        "src/",
    ],
    deps = [
        "@cgal//:cgal",
        ":CLI11",
        ":hypo-purecpp",
    ],
)
