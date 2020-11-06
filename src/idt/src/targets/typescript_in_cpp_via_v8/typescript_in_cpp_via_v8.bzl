load("@rules_haskell//haskell:defs.bzl", "haskell_binary")
load("@reify//src/idt/src/targets/typescript:typescript.bzl", "idt_as_typescript")
load("@reify//src/idt:rules.bzl", "IdtInfo")
load("@bazel_tools//tools/cpp:toolchain_utils.bzl", "find_cpp_toolchain")


def __idt_as_typescript_in_cpp_via_v8_rule_impl(ctx):
  output_directory_path = ctx.attr.idt[IdtInfo].namespace + "_typescript_in_cpp_via_v8"
  output_h_file = ctx.actions.declare_file(
       output_directory_path + "/reify_cpp_v8_interface.h")
  output_cc_file = ctx.actions.declare_file(
       output_directory_path + "/reify_cpp_v8_interface.cc")

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
          files = depset([
              output_h_file,
              output_cc_file])
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


def __generate_namespace_header_rule_impl(ctx):
  output_namespace_header = ctx.actions.declare_file(
      ctx.label.name + "/" + "reify_generated_project_namespace.h")
  ctx.actions.write(
      output = output_namespace_header,
      content = """
          #ifndef _REIFY_GENERATED_PROJECT_NAMESPACE_H_
          #define _REIFY_GENERATED_PROJECT_NAMESPACE_H_

          #define REIFY_GENERATED_PROJECT_NAMESPACE {namespace}

          #endif  // _REIFY_GENERATED_PROJECT_NAMESPACE_H_
      """.format(namespace=ctx.attr.idt[IdtInfo].namespace),
  )
  return [
      CcInfo(
          compilation_context = cc_common.create_compilation_context(
              headers = depset(direct = [output_namespace_header]),
              includes = depset(direct = [output_namespace_header.dirname]),
          ),
      ),
      DefaultInfo(
          files = depset([output_namespace_header])
      ) 
  ]


_generate_namespace_header_rule = rule(
  implementation = __generate_namespace_header_rule_impl,
  attrs = {
    "idt": attr.label(
        doc="The idt target that defines the interface to generate.",
        mandatory=True,),
  }
)

def idt_as_typescript_in_cpp_via_v8(name, idt, purecpp, ts_library_file):
  idt_typescript_name = name + "_typescript"
  idt_as_typescript(
    name=idt_typescript_name,
    idt=idt,
    library_file=ts_library_file,
  )

  typescript_to_header_name = name + "_typescript_to_header"
  native.genrule(
    name = typescript_to_header_name,
    srcs = [":" + idt_typescript_name],
    outs = ["src_gen/lib_ts.h", "src_gen/reify_generated_interface_ts.h"],
    cmd = """
        mkdir -p src_gen
        for f in $(SRCS)
        do
          case $$(basename $$f) in
            reify_generated_interface.ts)
              cp $$f $$(basename $$f)
              xxd -i $$(basename $$f) > $(RULEDIR)/src_gen/reify_generated_interface_ts.h
              rm $$(basename $$f)
              ;;
            *)
              cp $$f $$(basename $$f)
              xxd -i $$(basename $$f) > $(RULEDIR)/src_gen/lib_ts.h
              rm $$(basename $$f)
              ;;
          esac
        done    
    """,
  )
  typescript_to_header_lib_name = name + "_typescript_to_header_lib"
  native.cc_library(
    name = typescript_to_header_lib_name,
    hdrs = [":" + typescript_to_header_name],
    #data = [":" + typescript_to_header_name],
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

  interface_files_name = name + "_interface_files"
  _idt_as_typescript_in_cpp_via_v8_rule(
    name=interface_files_name,
    idt=idt,
    idt_typescript=":" + idt_typescript_name,
    generator_binary=generator_name,
  )

  # TODO: See if we can pull out the core code that does not need to be
  #       parameterized for each different IDT.
  namespace_header_name = name + "_namespace_header"
  _generate_namespace_header_rule(
      name=namespace_header_name,
      idt=idt,
  )
  core_name = name
  native.cc_library(
    name=core_name,
    srcs = [
        "@reify//src/idt/src/targets/typescript_in_cpp_via_v8:core/compiler_environment.cc",
        "@reify//src/idt/src/targets/typescript_in_cpp_via_v8:core/compiled_module.cc",
        "@reify//src/idt/src/targets/typescript_in_cpp_via_v8:core/compiled_module_impl.h",
        "@reify//src/idt/src/targets/typescript_in_cpp_via_v8:core/context_environment.h",
        "@reify//src/idt/src/targets/typescript_in_cpp_via_v8:core/generic_function.cc",
        "@reify//src/idt/src/targets/typescript_in_cpp_via_v8:core/generic_function_impl.h",
        "@reify//src/idt/src/targets/typescript_in_cpp_via_v8:core/global_initialization.cc",
        "@reify//src/idt/src/targets/typescript_in_cpp_via_v8:core/global_initialization.h",
        "@reify//src/idt/src/targets/typescript_in_cpp_via_v8:core/public_include/reify.h",
        "@reify//src/idt/src/targets/typescript_in_cpp_via_v8:core/runtime_environment.cc",
        "@reify//src/idt/src/targets/typescript_in_cpp_via_v8:core/typescript_compiler.cc",
        "@reify//src/idt/src/targets/typescript_in_cpp_via_v8:core/typescript_compiler.h",
        "@reify//src/idt/src/targets/typescript_in_cpp_via_v8:core/virtual_filesystem.cc",
        interface_files_name,
    ],
    deps = [
        purecpp,
        ":" + interface_files_name,
        ":" + namespace_header_name,
        ":" + typescript_to_header_lib_name,
        "@reify//src/idt/src/targets/typescript_in_cpp_via_v8:core",
        "@reify//src/tsc_wrapper:tsc_wrapper_as_header",
        "@v8//:v8",
    ],
  )
