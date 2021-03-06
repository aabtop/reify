load("@rules_haskell//haskell:defs.bzl", "haskell_library")

haskell_library(
    name = "target_logic",
    srcs = [
        "TargetCppV8.hs",
    ],
    compiler_flags = [
        "-XDuplicateRecordFields",
        "-XStandaloneDeriving",
        "-XTemplateHaskell",
        "-XOverloadedStrings",
    ],
    extra_srcs = [
        "templates/CppV8Enum.stache.h",
        "templates/CppV8Enum.stache.cc",
        "templates/CppV8SimpleEnum.stache.h",
        "templates/CppV8TaggedUnion.stache.h",
        "templates/CppV8TaggedUnion.stache.cc",
        "templates/CppV8Struct.stache.h",
        "templates/CppV8Struct.stache.cc",
        "templates/CppV8.stache.h",
        "templates/CppV8.stache.cc",
    ],
    visibility = ["//visibility:public"],
    deps = [
        "@reify//src/idt",
        "@stackage//:aeson",
        "@stackage//:base",
        "@stackage//:file-embed",
        "@stackage//:ghc",
        "@stackage//:megaparsec",
        "@stackage//:stache",
        "@stackage//:text",
    ],
)

cc_library(
    name = "core",
    srcs = [
        "core/compiled_module.cc",
        "core/compiled_module_impl.h",
        "core/compiler_environment.cc",
        "core/context_environment.h",
        "core/generic_function.cc",
        "core/generic_function_impl.h",
        "core/global_initialization.cc",
        "core/global_initialization.h",
        "core/public_include/reify/typescript_cpp_v8.h",
        "core/public_include/reify/typescript_cpp_v8/common_types.h",
        "core/runtime_environment.cc",
        "core/typescript_compiler.cc",
        "core/typescript_compiler.h",
        "core/virtual_filesystem.cc",
    ],
    includes = [
        "core",
        "core/public_include",
    ],
    visibility = ["//visibility:public"],
    deps = [
        "//src/tsc_wrapper:tsc_wrapper_as_header",
        "@v8",
    ],
)

cc_library(
    name = "command_line_tool",
    srcs = [
        "command_line_tool/command_line_tool.cc",
        "command_line_tool/include/reify/typescript_cpp_v8/command_line_tool.h",
    ],
    includes = [
        "command_line_tool/include",
    ],
    visibility = ["//visibility:public"],
    deps = [
        ":core",
        "//third_party:CLI11",
    ],
)

exports_files(["Main.hs"])
