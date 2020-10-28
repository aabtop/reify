load("@rules_foreign_cc//tools/build_defs:configure.bzl", "configure_make")

configure_make(
    name = "gmp",
    lib_source = "@gmp//:all",
    #shared_libraries = ["libgmp.so"],
    static_libraries = ["libgmp.a"],
    visibility = ["//visibility:public"],
)

configure_make(
    name = "mpfr",
    lib_source = "@mpfr//:all",
    #shared_libraries = ["libmpfr.so"],
    static_libraries = ["libmpfr.a"],
    visibility = ["//visibility:public"],
    deps = [":gmp"],
)

cc_library(
  name = "cgal",
  hdrs = glob([
    "include/CGAL/**/*.h",
    "include/CGAL/**/*.hpp",
  ]),
  includes = ["include"],
  visibility = ["//visibility:public"],
  deps = [
    "@boost//:core",
    "@boost//:predef",
    "@boost//:utility",
    "@boost//:array",
    "@boost//:iterator",
    "@boost//:lexical_cast",
    "@boost//:multiprecision",
    "@boost//:property_map",
    "@boost//:random",
    "@boost//:variant",
    "@boost//:any",
    "@boost//:graph",
    "@boost//:dynamic_bitset",
    "@boost//:tribool",
    "@boost//:heap",
  ] + select({
    "@bazel_tools//src/conditions:windows": [
        "@vcpkg//:mpir",
        "@vcpkg//:mpfr",
    ],
    "//conditions:default": [
      ":gmp",
      ":mpfr",
    ],
  }),
)
