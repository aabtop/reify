load("@npm_tsc_wrapper//webpack-cli:index.bzl", webpack = "webpack_cli")

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

genrule(
    name = "tsc_wrapper_convert_to_header",
    srcs = [":tsc_wrapper"],
    outs = ["src_gen/tsc_wrapper.h"],
    cmd = """
        mkdir -p src_gen
        cp $< $$(basename $<)
        xxd -i $$(basename $<) > $@
        rm $$(basename $<)
    """,
)
cc_library(
    name = "tsc_wrapper_as_header",
    hdrs = [":tsc_wrapper_convert_to_header"],
    includes = ["."],
    visibility = ["//visibility:public"],
)