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
        "src/third_party/CLI11/include",
    ],
    deps = [
        "@cgal//:cgal",
    ],
    data = [
        "@reify//src/tsc_wrapper",
        "@reify//src/third_party/idt"
    ],
)
