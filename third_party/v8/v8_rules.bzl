def def_v8_library_config(name, package_name):
  package_dir = "build_win/package/" + package_name

  native.cc_library(
    name=name + "_" + package_name,
    includes=[package_dir + "/include"],
    hdrs=native.glob([package_dir + "/include/**/*.h"]),
    srcs=native.glob([package_dir + "/lib/**"]),
    linkopts = select({
        "@bazel_tools//src/conditions:windows": [
            "/DEFAULTLIB:dbghelp.lib",
            "/DEFAULTLIB:winmm.lib"
        ],
        "//conditions:default": [],
    }),
    defines = [
      "V8_COMPRESS_POINTERS",
    ] + select({
          "@bazel_tools//src/conditions:windows": ["_ITERATOR_DEBUG_LEVEL=0",],
          "//conditions:default": [],
    }),
  )


def v8_library(name, visibility):
  def_v8_library_config(name, "win-debug-x64-msvc")
  def_v8_library_config(name, "win-release-x64-msvc")
  
  native.alias(
    name=name,
    actual=select({
      ":dbg_mode": name + "_win-debug-x64-msvc",
      "//conditions:default": name + "_win-release-x64-msvc",
    }),
    visibility=visibility,
  )
