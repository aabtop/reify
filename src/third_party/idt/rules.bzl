load("@rules_haskell//haskell:defs.bzl", "haskell_library", "haskell_binary")

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
      "@reify//src/third_party/idt",
    ],
  )
  _idt_rule(
    name=name,
    namespace=namespace,
    lib=name + "_lib",
  )


def __idt_as_purecpp_rule_impl(ctx):
  output_header_file = ctx.actions.declare_file(
       ctx.attr.idt[IdtInfo].namespace + "_purecpp" + "/" + ctx.attr.idt[IdtInfo].namespace + ".h")

  ctx.actions.run(
      outputs = [output_header_file],
      tools = [ctx.executable.generator_binary],
      arguments = [ctx.attr.idt[IdtInfo].namespace, output_header_file.dirname],
      progress_message = "Generating pure C++ interface for IDT %s" % ctx.attr.idt.label,
      #command = ctx.executable.generator_binary.path + ' ' + ' '.join([ctx.attr.idt[IdtInfo].namespace, output_directory.path]),
      executable=ctx.executable.generator_binary,
  )

  return [
      CcInfo(compilation_context = cc_common.create_compilation_context(
          headers = depset(direct = [output_header_file]),
          includes = depset(direct = [output_header_file.dirname]),
      )),
      DefaultInfo(
          files = depset([output_header_file])
      ) 
  ]


_idt_as_purecpp_rule = rule(
  implementation = __idt_as_purecpp_rule_impl,
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


def idt_as_purecpp(name, idt):
  generator_name = name + "_generator"
  haskell_binary(
    name=generator_name,
    srcs=["@reify//src/third_party/idt:src/targets/pure_cpp/Main.hs"],
    deps=[
      "@stackage//:base",
      "@stackage//:filepath",
      "@stackage//:directory",
      idt + "_lib",
      "@reify//src/third_party/idt",
    ],
  )
  _idt_as_purecpp_rule(
    name=name,
    idt=idt,
    generator_binary=generator_name,
  )
