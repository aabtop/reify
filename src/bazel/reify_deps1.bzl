"""Load dependencies needed to compile the protobuf library as a 3rd-party consumer."""

load("@bazel_tools//tools/build_defs/repo:http.bzl", "http_archive")
load("@bazel_tools//tools/build_defs/repo:git.bzl", "git_repository")
load("@reify//third_party/v8:v8_repository_rules.bzl", "fetch_v8")
load("@reify//third_party/qt:qt_repository_rules.bzl", "fetch_qt")

def reify_deps1():
    # Setup NPM repositories required to build the `tsc_wrapper`.
    http_archive(
        name = "build_bazel_rules_nodejs",
        urls = ["https://github.com/bazelbuild/rules_nodejs/releases/download/2.2.0/rules_nodejs-2.2.0.tar.gz"],
        sha256 = "4952ef879704ab4ad6729a29007e7094aef213ea79e9f2e94cbe1c9a753e63ef",
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

    fetch_v8(
        name = "v8",
        branch = "8.6.395.10",
    )

    fetch_qt(
        name = "qt",
    )