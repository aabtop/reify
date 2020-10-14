load("@rules_haskell//haskell:defs.bzl", "haskell_library")


IdtInfo = provider(fields=["namespace", "lib"])


def __idt_rule_impl(ctx):
  return [IdtInfo(namespace=ctx.attr.namespace, lib=ctx.attr.lib)]


_idt_rule = rule(
  implementation = __idt_rule_impl,
  attrs = {
      "namespace": attr.string(
          doc="The namespace of the interface, which code generators may use to prefix declarations with.",
          mandatory=True),
      "lib": attr.label(
          doc="Reference to a `haskell_library` target that ultimately export `ReifyInputInterface.idt`.",
          mandatory=True,
          providers=[],
      )
  },
)


def idt(name, srcs, namespace):
  lib_name = name + "_lib"
  haskell_library(
    name=lib_name,
    srcs=srcs,
    deps = [
      "@stackage//:base",
      "@reify//src/idt",
    ],
  )
  _idt_rule(
    name=name,
    namespace=namespace,
    lib=name + "_lib",
  )

