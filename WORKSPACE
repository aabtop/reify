workspace(
    name = "reify",
)

load("@bazel_tools//tools/build_defs/repo:http.bzl", "http_archive")
load("@bazel_tools//tools/build_defs/repo:git.bzl", "git_repository")

# Setup NPM repositories required to build the `tsc_wrapper`.
http_archive(
    name = "build_bazel_rules_nodejs",
    sha256 = "4952ef879704ab4ad6729a29007e7094aef213ea79e9f2e94cbe1c9a753e63ef",
    urls = ["https://github.com/bazelbuild/rules_nodejs/releases/download/2.2.0/rules_nodejs-2.2.0.tar.gz"],
)
load("@build_bazel_rules_nodejs//:index.bzl", "yarn_install")
yarn_install(
    name = "npm",
    package_json = "//src/tsc_wrapper:package.json",
    yarn_lock = "//src/tsc_wrapper:yarn.lock",
)

# Setup Haskell rules.
git_repository(
    name = "rules_haskell",
    # Corresponds to tag `v0.13`.
    commit = "c858a1d6c8d9866794762c6ca782841f55710907",
    remote = "https://github.com/tweag/rules_haskell.git",
)
load("@rules_haskell//haskell:repositories.bzl", "rules_haskell_dependencies")
rules_haskell_dependencies()

load("@rules_haskell//haskell:nixpkgs.bzl", "haskell_register_ghc_nixpkgs")

haskell_register_ghc_nixpkgs(
    attribute_path = "haskell.compiler.ghc865",
    repository = "@rules_haskell//nixpkgs:default.nix",
    version = "8.6.5",
)

load("@rules_haskell//haskell:toolchain.bzl", "rules_haskell_toolchains")

rules_haskell_toolchains(version = "8.6.5")

load(
    "@io_tweag_rules_nixpkgs//nixpkgs:nixpkgs.bzl",
    "nixpkgs_cc_configure",
    "nixpkgs_python_configure",
)

nixpkgs_cc_configure(
    nix_file = "@rules_haskell//nixpkgs:cc-toolchain.nix",
    nix_file_deps = ["@rules_haskell//nixpkgs:default.nix"],
    repository = "@rules_haskell//nixpkgs:default.nix",
)

nixpkgs_python_configure(
    repository = "@rules_haskell//nixpkgs:default.nix",
)


load(
    "@rules_haskell//haskell:cabal.bzl",
    "stack_snapshot"
)

stack_snapshot(
    name = "stackage",
    packages = [
        "base",
        "mtl",
        "containers",
        "filepath",
        "text",
        "ghc",
        "groom",
        "stache",
        "file-embed",
        "aeson",
        "megaparsec",
    ],
    # Last snapshot published for ghc-8.6.5 the default version picked up by
    # rules_haskell
    snapshot = "lts-14.27",
    # This uses an unpinned version of stack_snapshot, meaning that stack is invoked on every build.
    # To switch to pinned stackage dependencies, run `bazel run @stackage-unpinned//:pin` and
    # uncomment the following line.
    # stack_snapshot_json = "//:stackage_snapshot.json",
)
