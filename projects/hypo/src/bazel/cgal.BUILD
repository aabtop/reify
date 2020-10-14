cc_library(
  name = "cgal",
  hdrs = glob(["include/CGAL/**/*.h"]),
  includes = ["include"],
  visibility = ["//visibility:public"],
  deps = [
    "@boost//:core",
    "@boost//:predef",
  ],
  defines = [
    # performance-todo: CGAL is supposed to be faster with GMP, but it's
    # trickier to setup on Windows.
    "CGAL_DISABLE_GMP=1",
  ],
)
