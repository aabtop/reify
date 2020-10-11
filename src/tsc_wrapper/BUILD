load("@npm//webpack-cli:index.bzl", webpack = "webpack_cli")

webpack(
    name = "tsc_wrapper",
    outs = ["tsc_wrapper.js"],
    args = [
        "--config",
        "$(location webpack.config.js)",
        "-o",
        "$(location tsc_wrapper.js)",
    ],
    data = glob(["src/**/*.ts"]) + [
        "webpack.config.js",
        "@npm//@types/node",
        "@npm//typescript",
        "@npm//ts-loader",
        "@npm//raw-loader",
    ],
    visibility = ["//visibility:public"],
)
