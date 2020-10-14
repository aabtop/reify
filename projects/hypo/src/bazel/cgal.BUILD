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
    "@boost//:property_map",
    "@boost//:random",
    "@boost//:variant",
    "@boost//:any",
    "@boost//:graph",
    "@boost//:dynamic_bitset",
    "@boost//:tribool",
    "@boost//:heap",
  ],
  defines = [
    # performance-todo: CGAL is supposed to be faster with GMP, but it's
    # trickier to setup on Windows.
    "CGAL_DISABLE_GMP=1",
  ],
)
