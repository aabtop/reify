def _v8_library_impl(ctx):
    if "windows" in ctx.attr.os:
        output_filenames = ["v8_monolith.lib"]
        build_script_env = ctx.configuration.default_shell_env
        build_script_env.update({
            "ProgramFiles(x86)": "C:\\Program Files (x86)",
            "WINDIR": "C:\\Windows",
        })
        if ctx.attr.bazel_vc != "":
            build_script_env.update({
                "BAZEL_VC": ctx.attr.bazel_vc,
            })
    else:
        output_filenames = ["libv8_monolith.a"]
        build_script_env = {}
    output_files = [ctx.actions.declare_file(x) for x in output_filenames]

    ctx.actions.run(
        outputs = output_files,
        inputs = ctx.files.data,
        tools = [ctx.executable.build_script],
        arguments = [ctx.files.depot_tools_dir[0].path, ctx.files.v8_src_dir[0].path, ctx.attr.build_config] + [x.path for x in output_files],
        progress_message = "Building V8",
        executable = ctx.executable.build_script,
        env = build_script_env,
        execution_requirements = {"no-sandbox": "1"},
    )

    return [
        DefaultInfo(
            files = depset(output_files),
        ),
    ]

v8_library = rule(
    implementation = _v8_library_impl,
    attrs = {
        "build_script": attr.label(
            mandatory = True,
            allow_single_file = True,
            executable = True,
            cfg = "exec",
        ),
        "data": attr.label_list(
            mandatory = True,
        ),
        "depot_tools_dir": attr.label(mandatory = True, allow_single_file = True),
        "v8_src_dir": attr.label(mandatory = True, allow_single_file = True),
        "build_config": attr.string(mandatory = True),
        "os": attr.string(mandatory = True),
        "bazel_vc": attr.string(),
    },
)
