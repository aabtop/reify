load(
    "@rules_haskell//haskell:defs.bzl",
    "haskell_library",
)

haskell_library(
    name = "idt",
    srcs = [
      "src/Idt.hs",
      "src/IdtIdt.hs",
      "src/IdtProcessing.hs",
      "src/IdtStandardPipeline.hs",
      "src/Lib.hs",
      "src/TargetHaskell.hs",
    ],
    compiler_flags = [
      "-XDuplicateRecordFields",
      "-XStandaloneDeriving",
      "-XTemplateHaskell",
      "-XOverloadedStrings",
    ],
    visibility = ["//visibility:public"],
    deps = [
      "@stackage//:base",
      "@stackage//:mtl",
      "@stackage//:containers",
      "@stackage//:filepath",
      "@stackage//:text",
      "@stackage//:ghc",
      "@stackage//:stache",
      "@stackage//:file-embed",
      "@stackage//:aeson",
      "@stackage//:megaparsec",
    ],
)

