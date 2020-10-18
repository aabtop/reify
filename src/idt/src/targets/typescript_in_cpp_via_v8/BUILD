load("@rules_haskell//haskell:defs.bzl", "haskell_library")

haskell_library(
  name = "target_logic",
  srcs = [
    "TargetCppV8.hs",
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

# TODO: Ideally most of the code that is not IDT-specific will go here, but
#       currently this is just providing the include directory to dependencies.
cc_library(
  name = "core",
  includes=[
    "core",
    "core/public_include",
  ],
  visibility = ["//visibility:public"],
)

exports_files(["Main.hs"] + glob(["core/**"]))
