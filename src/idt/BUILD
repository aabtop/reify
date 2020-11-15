load(
    "@rules_haskell//haskell:defs.bzl",
    "haskell_library",
)

haskell_library(
    name = "idt",
    srcs = [
        "core/Idt.hs",
        "core/IdtIdt.hs",
        "core/IdtProcessing.hs",
        "core/IdtStandardPipeline.hs",
        "core/Lib.hs",
        "core/TargetHaskell.hs",
    ],
    compiler_flags = [
        "-XDuplicateRecordFields",
        "-XStandaloneDeriving",
        "-XTemplateHaskell",
        "-XOverloadedStrings",
    ],
    visibility = ["//visibility:public"],
    deps = [
        "@stackage//:aeson",
        "@stackage//:base",
        "@stackage//:containers",
        "@stackage//:file-embed",
        "@stackage//:filepath",
        "@stackage//:ghc",
        "@stackage//:megaparsec",
        "@stackage//:mtl",
        "@stackage//:stache",
        "@stackage//:text",
    ],
)
