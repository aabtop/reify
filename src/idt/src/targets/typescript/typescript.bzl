load("@rules_haskell//haskell:defs.bzl", "haskell_binary")
load("@reify//src/idt:rules.bzl", "IdtInfo")


def __idt_as_typescript_impl(ctx):
  output_ts_file = ctx.actions.declare_file(
       ctx.attr.idt[IdtInfo].namespace + "_typescript/reify_generated_interface.ts")

  ctx.actions.run(
      outputs = [output_ts_file],
      tools = [ctx.executable.generator_binary],
      arguments = [output_ts_file.dirname],
      progress_message = "Generating TypeScript interface for IDT %s" % ctx.attr.idt.label,
      executable=ctx.executable.generator_binary,
  )

  return [
      DefaultInfo(
          files = depset([output_ts_file])
      ) 
  ]


_idt_as_typescript_rule = rule(
  implementation = __idt_as_typescript_impl,
  attrs = {
    "idt": attr.label(
        doc="The idt target that defines the interface to generate.",
        mandatory=True,),
    "generator_binary": attr.label(
        doc="Haskell binary that we can run to generate the C++ file.",
        mandatory=True,
        allow_files = True,
        executable = True,
        cfg = "exec",),
  },
)


def idt_as_typescript(name, idt):
  generator_name = name + "_generator"
  haskell_binary(
    name=generator_name,
    srcs=[
      "@reify//src/idt/src/targets/typescript:Main.hs",
    ],
    deps=[
      "@stackage//:base",
      "@stackage//:filepath",
      "@stackage//:directory",
      "@reify//src/idt",
      "@reify//src/idt/src/targets/typescript",
      idt + "_lib",
    ],
  )
  _idt_as_typescript_rule(
    name=name,
    idt=idt,
    generator_binary=generator_name,
  )
