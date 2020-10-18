load("@rules_haskell//haskell:defs.bzl", "haskell_binary")
load("@reify//src/idt/src/targets/typescript:typescript.bzl", "idt_as_typescript")
load("@reify//src/idt:rules.bzl", "IdtInfo")
load("@bazel_tools//tools/cpp:toolchain_utils.bzl", "find_cpp_toolchain")


def __idt_as_typescript_in_cpp_via_v8_rule_impl(ctx):
  output_directory_path = ctx.attr.idt[IdtInfo].namespace + "_typescript_in_cpp_via_v8"
  output_h_file = ctx.actions.declare_file(
       output_directory_path + "/" + ctx.attr.idt[IdtInfo].namespace + "_v8.h")
  output_cc_file = ctx.actions.declare_file(
       output_directory_path + "/" + ctx.attr.idt[IdtInfo].namespace + "_v8.cc")

  ctx.actions.run(
      outputs = [output_h_file, output_cc_file],
      tools = [ctx.executable.generator_binary],
      arguments = [ctx.attr.idt[IdtInfo].namespace, output_h_file.dirname],
      progress_message = "Generating C++ V8 <-> TypeScript interface for IDT %s" % ctx.attr.idt.label,
      executable=ctx.executable.generator_binary,
  )


  return [
      CcInfo(
          compilation_context = cc_common.create_compilation_context(
              headers = depset(direct = [output_h_file]),
              includes = depset(direct = [output_h_file.dirname]),
          ),
      ),
      DefaultInfo(
          files = depset([output_h_file, output_cc_file])
      ) 
  ]


_idt_as_typescript_in_cpp_via_v8_rule = rule(
  implementation = __idt_as_typescript_in_cpp_via_v8_rule_impl,
  attrs = {
    "idt": attr.label(
        doc="The idt target that defines the interface to generate.",
        mandatory=True,),
    "idt_typescript": attr.label(
        doc="TypeScript translation of the IDT target we're managing.",
        mandatory=True,),
    "generator_binary": attr.label(
        doc="Haskell binary that we can run to generate the C++ file.",
        mandatory=True,
        allow_files = True,
        executable = True,
        cfg = "exec",),
  },
)


def idt_as_typescript_in_cpp_via_v8(name, idt, purecpp):
  idt_typescript_name = name + "_typescript"
  idt_as_typescript(
    name=idt_typescript_name,
    idt=idt,
  )

  generator_name = name + "_generator"
  haskell_binary(
    name=generator_name,
    srcs=[
      "@reify//src/idt/src/targets/typescript_in_cpp_via_v8:Main.hs",
    ],
    deps=[
      "@stackage//:base",
      "@stackage//:filepath",
      "@stackage//:directory",
      "@reify//src/idt",
      "@reify//src/idt/src/targets/typescript_in_cpp_via_v8:target_logic",
      idt + "_lib",
    ],
  )

  interface_name = name + "_interface"
  _idt_as_typescript_in_cpp_via_v8_rule(
    name=interface_name,
    idt=idt,
    idt_typescript=":" + idt_typescript_name,
    generator_binary=generator_name,
  )

  native.cc_library(
    name=name,
    srcs = [interface_name],
    deps = [
      interface_name,
      purecpp,
      "@reify//third_party/v8",
    ],
  )
