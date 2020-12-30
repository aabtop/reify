load("@npm_monaco_wrapper//webpack-cli:index.bzl", webpack = "webpack_cli")

webpack(
    name = "monaco_wrapper",
    outs = [
        "editor.worker.js",
        "index.html",
        "monaco_wrapper.js",
        "ts.worker.js",
    ],
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
        "@npm_monaco_wrapper//style-loader",
        "@npm_monaco_wrapper//ts-loader",
        "@npm_monaco_wrapper//css-loader",
        "@npm_monaco_wrapper//monaco-editor",
        "@npm_monaco_wrapper//monaco-editor-webpack-plugin",
        "@npm_monaco_wrapper//copy-webpack-plugin",
        "@npm_monaco_wrapper//base64-inline-loader",
        "@npm_monaco_wrapper//webpack",
    ],
    visibility = ["//visibility:public"],
)
