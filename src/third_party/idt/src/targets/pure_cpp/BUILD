load("@rules_haskell//haskell:defs.bzl", "haskell_library")

haskell_library(
  name = "target_pure_cpp_library",
  srcs = [
    "TargetCppImmutableRefCounted.hs",
  ],
  compiler_flags = [
    "-XDuplicateRecordFields",
    "-XStandaloneDeriving",
    "-XTemplateHaskell",
    "-XOverloadedStrings",
  ],
  deps=[
    "@stackage//:base",
    "@stackage//:stache",
    "@stackage//:text",
    "@stackage//:file-embed",
    "@stackage//:aeson",
    "@stackage//:megaparsec",
    "@stackage//:ghc",
    "@reify//src/third_party/idt",
  ],
  visibility = ["//visibility:public"],
)

exports_files(["Main.hs"])
