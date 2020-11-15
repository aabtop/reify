load("@rules_haskell//haskell:defs.bzl", "haskell_library")

haskell_library(
  name = "pure_cpp",
  srcs = [
    "TargetCppImmutableRefCounted.hs",
  ],
  extra_srcs = [
    "templates/CppImmutableRefCounted.stache.h",
    "templates/CppImmutableRefCountedEnum.stache.h",
    "templates/CppImmutableRefCountedStruct.stache.h",
    "templates/CppImmutableRefCountedTaggedUnion.stache.h",
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
    "@reify//src/idt",
  ],
  visibility = ["//visibility:public"],
)

exports_files(["Main.hs"])
