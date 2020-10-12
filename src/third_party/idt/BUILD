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
      "src/TargetCppImmutableRefCounted.hs",
      "src/TargetCppV8.hs",
      "src/TargetHaskell.hs",
      "src/TargetTypeScript.hs",
    ],
    extra_srcs = [
      # Template files.
      "src/CppImmutableRefCounted.stache.h",
      "src/CppImmutableRefCountedEnum.stache.h",
      "src/CppImmutableRefCountedStruct.stache.h",
      "src/CppImmutableRefCountedTaggedUnion.stache.h",
      "src/CppV8.stache.cc",
      "src/CppV8.stache.h",
      "src/CppV8Enum.stache.cc",
      "src/CppV8Enum.stache.h",
      "src/CppV8SimpleEnum.stache.h",
      "src/CppV8Struct.stache.cc",
      "src/CppV8Struct.stache.h",
      "src/CppV8TaggedUnion.stache.cc",
      "src/CppV8TaggedUnion.stache.h",
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
