load("@reify//:src/bazel/v8/build_v8.bzl", "v8_library")

v8_library(
  name="v8",
  v8_src_dir=".",
  gn="@v8_depot_tools//:gn",
  visibility = ["//visibility:public"],
)
