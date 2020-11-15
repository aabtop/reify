load("@rules_haskell//haskell:defs.bzl", "haskell_library")

haskell_library(
  name = "typescript",
  srcs = [
    "TargetTypeScript.hs",
  ],
  compiler_flags = [
    "-XDuplicateRecordFields",
    "-XStandaloneDeriving",
    "-XTemplateHaskell",
    "-XOverloadedStrings",
  ],
  deps=[
    "@stackage//:base",
    "@stackage//:text",
    "@stackage//:ghc",
    "@reify//src/idt",
  ],
  visibility = ["//visibility:public"],
)

exports_files(["Main.hs"])
