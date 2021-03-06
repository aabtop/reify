load("@bazel_skylib//lib:dicts.bzl", "dicts")
load("@build_bazel_rules_nodejs//:index.bzl", "yarn_install")
load("@rules_haskell//haskell:repositories.bzl", "rules_haskell_dependencies")
load("@rules_haskell//haskell:toolchain.bzl", "rules_haskell_toolchains")
load("@rules_haskell//haskell:cabal.bzl", "stack_snapshot")
load("@rules_sh//sh:posix.bzl", "sh_posix_configure")
load("@aabtop_rules_qt//:rules_qt_deps1.bzl", "rules_qt_deps1")

def reify_deps2():
    yarn_install(
        name = "npm_tsc_wrapper",
        package_json = "@reify//src/tsc_wrapper:package.json",
        yarn_lock = "@reify//src/tsc_wrapper:yarn.lock",
        frozen_lockfile = True,
        symlink_node_modules = False,
    )
    yarn_install(
        name = "npm_monaco_wrapper",
        package_json = "@reify//src/monaco_wrapper:package.json",
        yarn_lock = "@reify//src/monaco_wrapper:yarn.lock",
        frozen_lockfile = True,
        symlink_node_modules = False,
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

    rules_qt_deps1()
