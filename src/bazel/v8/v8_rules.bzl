def v8_library(name, visibility):
  package_dir = "build_win/package/win-release-x64-msvc"

  native.cc_library(
    name=name,
    includes=[package_dir + "/include"],
    hdrs=native.glob([package_dir + "/include/**/*.h"]),
    srcs=native.glob([package_dir + "/lib/**"]),
    visibility=visibility,
  )
