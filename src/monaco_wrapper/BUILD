load("@npm_monaco_wrapper//webpack-cli:index.bzl", webpack = "webpack_cli")

webpack(
    name = "monaco_wrapper",
    outs = ["monaco_wrapper.js", "index.html", "ts.worker.js"],
    args = [
        "--config",
        "$(location webpack.config.js)",
        "-o",
        "$(location monaco_wrapper.js)",
    ],
    data = glob(["src/**"]) + [
        "package.json",
        "tsconfig.json",
        "yarn.lock",
        "webpack.config.js",
        "@npm_monaco_wrapper//typescript",
        "@npm_monaco_wrapper//ts-loader",
        "@npm_monaco_wrapper//monaco-editor-webpack-plugin",
        "@npm_monaco_wrapper//copy-webpack-plugin",
    ],
    visibility = ["//visibility:public"],
)
