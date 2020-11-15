load("@rules_haskell//haskell:defs.bzl", "haskell_binary")
load("@reify//src/idt/src/targets/typescript:typescript.bzl", "idt_as_typescript")
load("@reify//src/idt:rules.bzl", "IdtInfo")
load("@bazel_tools//tools/cpp:toolchain_utils.bzl", "find_cpp_toolchain")

def __idt_as_typescript_cpp_v8_rule_impl(ctx):
    output_directory = "reify/typescript_cpp_v8/"
    output_h_file = ctx.actions.declare_file(
        output_directory + ctx.attr.idt[IdtInfo].namespace + ".h",
    )
    output_cc_file = ctx.actions.declare_file(
        output_directory + ctx.attr.idt[IdtInfo].namespace + ".cc",
    )

    ctx.actions.run(
        outputs = [output_h_file, output_cc_file],
        tools = [ctx.executable.generator_binary],
        arguments = [ctx.attr.idt[IdtInfo].namespace, ctx.bin_dir.path + "/" + output_directory],
        progress_message = "Generating C++ V8 <-> TypeScript interface for IDT %s" % ctx.attr.idt.label,
        executable = ctx.executable.generator_binary,
    )

    return [
        CcInfo(
            compilation_context = cc_common.create_compilation_context(
                headers = depset(direct = [output_h_file]),
                includes = depset(direct = [ctx.bin_dir.path]),
            ),
        ),
        DefaultInfo(
            files = depset([
                output_h_file,
                output_cc_file,
            ]),
        ),
    ]

_idt_as_typescript_cpp_v8_rule = rule(
    implementation = __idt_as_typescript_cpp_v8_rule_impl,
    attrs = {
        "idt": attr.label(
            doc = "The idt target that defines the interface to generate.",
            mandatory = True,
        ),
        "idt_typescript": attr.label(
            doc = "TypeScript translation of the IDT target we're managing.",
            mandatory = True,
        ),
        "generator_binary": attr.label(
            doc = "Haskell binary that we can run to generate the C++ file.",
            mandatory = True,
            allow_files = True,
            executable = True,
            cfg = "exec",
        ),
    },
)

def idt_as_typescript_cpp_v8(name, idt, purecpp, typescript):
    typescript_to_header_name = name + "_typescript_to_header"
    native.genrule(
        name = typescript_to_header_name,
        srcs = [typescript],
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
              cp $$f lib.ts
              xxd -i lib.ts > $(RULEDIR)/src_gen/lib_ts.h
              rm lib.ts
              ;;
          esac
        done    
    """,
    )
    typescript_to_header_lib_name = name + "_typescript_to_header_lib"
    native.cc_library(
        name = typescript_to_header_lib_name,
        hdrs = [":" + typescript_to_header_name],
    )

    generator_name = name + "_generator"
    haskell_binary(
        name = generator_name,
        srcs = [
            "@reify//src/idt/src/targets/typescript_cpp_v8:Main.hs",
        ],
        deps = [
            "@stackage//:base",
            "@stackage//:filepath",
            "@stackage//:directory",
            "@reify//src/idt",
            "@reify//src/idt/src/targets/typescript_cpp_v8:target_logic",
            idt + "_lib",
        ],
    )

    interface_files_name = name + "_interface_files"
    _idt_as_typescript_cpp_v8_rule(
        name = interface_files_name,
        idt = idt,
        idt_typescript = typescript,
        generator_binary = generator_name,
    )

    native.cc_library(
        name = name,
        srcs = [
            interface_files_name,
        ],
        deps = [
            purecpp,
            ":" + interface_files_name,
            ":" + typescript_to_header_lib_name,
            "@reify//src/idt/src/targets/typescript_cpp_v8:core",
            "@v8//:v8",
        ],
    )
