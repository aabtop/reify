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