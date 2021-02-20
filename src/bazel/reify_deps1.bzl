"""Load dependencies needed to compile the protobuf library as a 3rd-party consumer."""

load("@bazel_tools//tools/build_defs/repo:http.bzl", "http_archive")
load("@bazel_tools//tools/build_defs/repo:git.bzl", "git_repository")
load("@reify//third_party/v8:v8_repository_rules.bzl", "fetch_v8")
load("@reify//third_party/qt:qt_repository_rules.bzl", "setup_qt")

def reify_deps1():
    # Setup NPM repositories required to build the `tsc_wrapper`.
    http_archive(
        name = "build_bazel_rules_nodejs",
        sha256 = "fcc6dccb39ca88d481224536eb8f9fa754619676c6163f87aa6af94059b02b12",
        urls = ["https://github.com/bazelbuild/rules_nodejs/releases/download/3.2.0/rules_nodejs-3.2.0.tar.gz"],
    )

    # Setup Haskell rules.
    http_archive(
        name = "rules_haskell",
        strip_prefix = "rules_haskell-0.13",
        urls = ["https://github.com/tweag/rules_haskell/archive/v0.13.tar.gz"],
        sha256 = "b4e2c00da9bc6668fa0404275fecfdb31beb700abdba0e029e74cacc388d94d6",
    )

    git_repository(
        name = "rules_sh",
        remote = "https://github.com/tweag/rules_sh.git",
        # Corresponds to tag "v0.2.0"
        commit = "c6af1fe30e5b83a043d8543defc292b90750e236",
        shallow_since = "1585235671 +0000",
    )

    git_repository(
        name = "bazel_skylib",
        remote = "https://github.com/bazelbuild/bazel-skylib",
        # Corresponds to tag "1.0.3"
        commit = "2ec2e6d715e993d96ad6222770805b5bd25399ae",
        shallow_since = "1598536904 -0400",
    )

    http_archive(
        name = "com_github_zaucy_rules_7zip",
        strip_prefix = "rules_7zip-e95ba876db445cf2c925c02c4bc18ed37a503fd8",
        url = "https://github.com/zaucy/rules_7zip/archive/e95ba876db445cf2c925c02c4bc18ed37a503fd8.zip",
        sha256 = "b66e1c712577b0c029d4c94228dba9c8aacdcdeb88c3b1eeeffd00247ba5a856",
    )

    git_repository(
        name = "com_github_zaucy_rules_vulkan",
        remote = "https://github.com/zaucy/rules_vulkan",
        commit = "ebfb9377f616cf12ffe0a9e1088ca0c005bd2db4",
        shallow_since = "1611480038 +0000",
    )

    fetch_v8(
        name = "v8",
        branch = "8.6.395.10",
    )

    http_archive(
        name = "fmtlib",
        strip_prefix = "fmt-7.1.3",
        url = "https://github.com/fmtlib/fmt/archive/7.1.3.zip",
        sha256 = "50f2fd9f697f89726ae3c7efe84ae48c9e03158a2958eea496eeaa0fb190adb6",
        build_file = "@reify//:third_party/fmt/fmtlib.BUILD",
    )

    http_archive(
        name = "glm",
        strip_prefix = "glm-0.9.9.8",
        url = "https://github.com/g-truc/glm/archive/0.9.9.8.zip",
        sha256 = "4605259c22feadf35388c027f07b345ad3aa3b12631a5a316347f7566c6f1839",
        build_file = "@reify//:third_party/glm/glm.BUILD",
    )
    setup_qt()
