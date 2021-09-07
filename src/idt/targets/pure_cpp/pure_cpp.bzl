load("@rules_haskell//haskell:defs.bzl", "haskell_binary")
load("@reify//src/idt:rules.bzl", "IdtInfo")

def __idt_as_purecpp_rule_impl(ctx):
    output_directory = "reify/purecpp/"
    output_header_file = ctx.actions.declare_file(
        output_directory + ctx.attr.idt[IdtInfo].namespace + ".h",
    )

    ctx.actions.run(
        outputs = [output_header_file],
        tools = [ctx.executable.generator_binary],
        arguments = [
            ctx.attr.idt[IdtInfo].namespace,
            ctx.bin_dir.path + "/" + output_directory,
            "True" if ctx.attr.enable_hashes else "False",
        ],
        progress_message = "Generating pure C++ interface for IDT %s" % ctx.attr.idt.label,
        executable = ctx.executable.generator_binary,
    )

    return [
        CcInfo(compilation_context = cc_common.create_compilation_context(
            headers = depset(direct = [output_header_file]),
            includes = depset(direct = [ctx.bin_dir.path]),
        )),
        DefaultInfo(
            files = depset([output_header_file]),
        ),
    ]

_idt_as_purecpp_rule = rule(
    implementation = __idt_as_purecpp_rule_impl,
    attrs = {
        "idt": attr.label(
            doc = "The idt target that defines the interface to generate.",
            mandatory = True,
        ),
        "generator_binary": attr.label(
            doc = "Haskell binary that we can run to generate the C++ file.",
            mandatory = True,
            allow_files = True,
            executable = True,
            cfg = "exec",
        ),
        "enable_hashes": attr.bool(
            default = False,
            doc = "When set to true, will generate hash functions for all data types, and references will cache the hash of their referenced value.",
        )
    },
)

def idt_as_purecpp(name, idt, enable_hashes=False):
    generator_name = name + "_generator"
    haskell_binary(
        name = generator_name,
        srcs = [
            "@reify//src/idt/targets/pure_cpp:Main.hs",
        ],
        deps = [
            "@stackage//:base",
            "@stackage//:filepath",
            "@stackage//:directory",
            "@reify//src/idt",
            "@reify//src/idt/targets/pure_cpp",
            idt + "_lib",
        ],
    )

    purecpp_source_files_name = name + "_source_files"
    _idt_as_purecpp_rule(
        name = purecpp_source_files_name,
        idt = idt,
        generator_binary = generator_name,
        enable_hashes = enable_hashes,
    )

    target_deps = []
    if enable_hashes:
        # We use BLAKE3 for hashing:
        # https://github.com/BLAKE3-team/BLAKE3
        target_deps += ["@BLAKE3"]

    native.cc_library(
        name = name,
        srcs = [],
        deps = [
            ":" + purecpp_source_files_name,
        ] + target_deps,
    )
