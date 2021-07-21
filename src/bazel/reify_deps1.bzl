"""Load dependencies needed to compile the protobuf library as a 3rd-party consumer."""

load("@bazel_tools//tools/build_defs/repo:http.bzl", "http_archive")
load("@bazel_tools//tools/build_defs/repo:git.bzl", "git_repository")
load("@reify//third_party/v8:v8_repository_rules.bzl", "fetch_v8")

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

    http_archive(
        name = "aabtop_rules_qt",
        strip_prefix = "rules_qt-8c56615696d7bc4873a2afb011b7bf032867be89",
        url = "https://github.com/aabtop/rules_qt/archive/8c56615696d7bc4873a2afb011b7bf032867be89.zip",
        sha256 = "50dbb11fc0a51bdbad322db038383d146478d4dbd95011e7b1e75cfa27692734",
    )

    http_archive(
        name = "imgui",
        strip_prefix = "imgui-1ad1429c6df657f9694b619d53fa0e65e482f32b",
        url = "https://github.com/ocornut/imgui/archive/1ad1429c6df657f9694b619d53fa0e65e482f32b.zip",
        sha256 = "08213444a7b93999494921215807960f39255509d320c6923270ba698f5c4e56",
        build_file = "@reify//:third_party/imgui/imgui.BUILD",
    )

    http_archive(
        name = "platform_window",
        strip_prefix = "platform_window-9c43555ae010a331007b1afc5c3509d288168b38",
        urls = ["https://github.com/aabtop/platform_window/archive/9c43555ae010a331007b1afc5c3509d288168b38.zip"],
        sha256 = "4d6c8271f92d09ef8c8a1081be6fa505a97acc9d56aa15bfc915542435fdab26",
    )

    http_archive(
        name = "imgui-filebrowser",
        strip_prefix = "imgui-filebrowser-02be68a5d5c6bc4ab1d18fabe0963dc737b226a6",
        urls = ["https://github.com/AirGuanZ/imgui-filebrowser/archive/02be68a5d5c6bc4ab1d18fabe0963dc737b226a6.zip"],
        sha256 = "dc7dda7aeadc65e1201c8b6210d82bb146de957a582930d14650d544d165ae34",
        build_file = "@reify//:third_party/imgui-filebrowser/imgui-filebrowser.BUILD",
    )
