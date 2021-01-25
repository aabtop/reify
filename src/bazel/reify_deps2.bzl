load("@bazel_skylib//lib:dicts.bzl", "dicts")
load("@build_bazel_rules_nodejs//:index.bzl", "yarn_install")
load("@rules_haskell//haskell:repositories.bzl", "rules_haskell_dependencies")
load("@rules_haskell//haskell:toolchain.bzl", "rules_haskell_toolchains")
load("@rules_haskell//haskell:cabal.bzl", "stack_snapshot")
load("@rules_sh//sh:posix.bzl", "sh_posix_configure")
load("@com_github_zaucy_rules_7zip//:setup.bzl", "setup_7zip")
load("@com_github_zaucy_rules_vulkan//:repo.bzl", "vulkan_repos")
load("@os_specific_vulkan_sdk_rules//:current_os_repo.bzl", "setup_os_specific_vulkan_repos")
load("@reify//third_party/qt:qt_repository_rules.bzl", "fetch_qt")

def reify_deps2():
    yarn_install(
        name = "npm_tsc_wrapper",
        package_json = "@reify//src/tsc_wrapper:package.json",
        yarn_lock = "@reify//src/tsc_wrapper:yarn.lock",
    )
    yarn_install(
        name = "npm_monaco_wrapper",
        package_json = "@reify//src/monaco_wrapper:package.json",
        yarn_lock = "@reify//src/monaco_wrapper:yarn.lock",
    )

    sh_posix_configure()

    stack_snapshot(
        name = "stackage",
        packages = [
            "base",
            "mtl",
            "containers",
            "directory",
            "filepath",
            "text",
            "ghc",
            "groom",
            "stache",
            "file-embed",
            "aeson",
            "megaparsec",
            "process",
        ],
        # Last snapshot published for ghc-8.6.5 the default version picked up by
        # rules_haskell
        snapshot = "lts-14.27",
    )

    rules_haskell_dependencies()
    rules_haskell_toolchains(version = "8.6.5")

    setup_7zip()
    vulkan_repos()

    setup_os_specific_vulkan_repos()
    fetch_qt(
        name = "qt",
    )
