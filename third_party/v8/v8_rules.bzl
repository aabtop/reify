def _v8_library_impl(ctx):
    ctx.actions.run(
        outputs = [ctx.outputs.lib],
        inputs = ctx.files.data,
        tools = [ctx.executable._build_script],
        arguments = [ctx.files.depot_tools_dir[0].path, ctx.files.v8_src_dir[0].path, ctx.attr.build_config, ctx.outputs.lib.path],
        progress_message = "Building V8",
        executable = ctx.executable._build_script,
        execution_requirements = {"no-sandbox": "1"},
    )

    return [
        DefaultInfo(
            files = depset([ctx.outputs.lib]),
        ),
    ]

v8_library = rule(
    implementation = _v8_library_impl,
    attrs = {
        "_build_script": attr.label(
            default = "@reify//third_party/v8:build_linux.sh",
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
    },
    outputs = {
        "lib": "libv8_monolith.a",
    },
)
