load("@npm_tsc_wrapper//webpack-cli:index.bzl", webpack = "webpack_cli")
load("//:src/bazel/utils.bzl", "convert_to_header")

webpack(
    name = "tsc_wrapper",
    outs = ["tsc_wrapper.js"],
    args = [
        "--config",
        "$(location webpack.config.js)",
        "-o",
        "$@",
    ],
    data = glob(["src/**/*.ts"]) + [
        "package.json",
        "tsconfig.json",
        "yarn.lock",
        "webpack.config.js",
        "@npm_tsc_wrapper//@types/node",
        "@npm_tsc_wrapper//typescript",
        "@npm_tsc_wrapper//ts-loader",
        "@npm_tsc_wrapper//raw-loader",
        "@npm_tsc_wrapper//webpack",
        "@npm_tsc_wrapper//webpack-cli",
        "@npm_tsc_wrapper//:node_modules",
    ],
    visibility = ["//visibility:public"],
)

convert_to_header("tsc_wrapper_as_header", ":tsc_wrapper", "tsc_wrapper.h")
