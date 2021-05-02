workspace(
    name = "reify",
)

load("@reify//:src/bazel/reify_deps1.bzl", "reify_deps1")

reify_deps1()

load("@reify//:src/bazel/reify_deps2.bzl", "reify_deps2")

reify_deps2()

load("@reify//:src/bazel/reify_deps3.bzl", "reify_deps3")

reify_deps3()

# The rest of this is tooling for unit testing.
load("@bazel_tools//tools/build_defs/repo:http.bzl", "http_archive")

http_archive(
    name = "com_google_googletest",
    sha256 = "5cf189eb6847b4f8fc603a3ffff3b0771c08eec7dd4bd961bfd45477dd13eb73",
    strip_prefix = "googletest-609281088cfefc76f9d0ce82e1ff6c30cc3591e5",
    urls = ["https://github.com/google/googletest/archive/609281088cfefc76f9d0ce82e1ff6c30cc3591e5.zip"],
)
