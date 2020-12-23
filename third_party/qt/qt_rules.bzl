def __qt_compile_ui_impl(ctx):
    ctx.actions.run(
        outputs = [ctx.outputs.out],
        inputs = ctx.files.ui_src,
        tools = [ctx.executable.uic],
        arguments = [ctx.files.ui_src[0].path, "-o", ctx.outputs.out.path],
        executable = ctx.executable.uic,
    )

    return DefaultInfo(files = depset([ctx.outputs.out]))

_qt_compile_ui = rule(
    implementation = __qt_compile_ui_impl,
    attrs = {
        "uic": attr.label(default = "@qt//:uic", allow_single_file = True, executable = True, cfg = "exec"),
        "ui_src": attr.label(mandatory = True, allow_single_file = True),
        "out": attr.output(mandatory = True),
    },
)

def qt_ui_header(name, ui_src):
    ui_header_file = "ui_{}.h".format(ui_src.split(".")[0])
    uic_header_target_name = "{}_uic".format(name)
    _qt_compile_ui(
        name = uic_header_target_name,
        ui_src = ui_src,
        out = ui_header_file,
    )

    native.cc_library(
        name = name,
        hdrs = [ui_header_file],
    )

def __qt_compile_moc_impl(ctx):
    ctx.actions.run(
        outputs = [ctx.outputs.out],
        inputs = ctx.files.hdr_src,
        tools = [ctx.executable.moc],
        arguments = [ctx.files.hdr_src[0].path, "-o", ctx.outputs.out.path, "-f\"{}\"".format(ctx.files.hdr_src[0].path)],
        executable = ctx.executable.moc,
    )

    return DefaultInfo(files = depset([ctx.outputs.out]))

_qt_compile_moc = rule(
    implementation = __qt_compile_moc_impl,
    attrs = {
        "moc": attr.label(default = "@qt//:moc", allow_single_file = True, executable = True, cfg = "exec"),
        "hdr_src": attr.label(mandatory = True, allow_single_file = True),
        "out": attr.output(mandatory = True),
        "package_name": attr.string(mandatory = True),
    },
)

def qt_moc_src(name, hdr_src):
    moc_src_file = "moc_{}.cc".format(hdr_src.split(".")[0])
    moc_target_name = "{}_moc".format(name)
    _qt_compile_moc(
        name = name,
        hdr_src = hdr_src,
        out = moc_src_file,
        package_name = native.package_name(),
    )

def qt_cc_library(name, srcs, hdr, ui_src, qt_dep, deps = [], **kwargs):
    moc_target_name = "{}_moc".format(name)
    qt_moc_src(moc_target_name, hdr)

    uic_target_name = "{}_ui".format(name)
    qt_ui_header(uic_target_name, ui_src)

    native.cc_library(
        name = name,
        srcs = srcs + [":" + moc_target_name],
        hdrs = [hdr],
        deps = [qt_dep + "_lib"] + deps + [":" + uic_target_name],
        **kwargs
    )

def __package_runtime_files_impl(ctx):
    local_files = []
    for x in ctx.files.runtime_files:
        new_local_file = ctx.actions.declare_file(x.path.replace(ctx.attr.runtime_files.label.workspace_root + "/", ""))
        ctx.actions.symlink(output = new_local_file, target_file = x)
        local_files.append(new_local_file)

    for x in ctx.files.runtime_sibling_files:
        new_local_file = ctx.actions.declare_file(x.basename)
        ctx.actions.symlink(output = new_local_file, target_file = x)
        local_files.append(new_local_file)

    for x in ctx.files.runtime_qt_platforms_plugins_files:
        new_local_file = ctx.actions.declare_file("platforms/" + x.basename)
        ctx.actions.symlink(output = new_local_file, target_file = x)
        local_files.append(new_local_file)

    return [DefaultInfo(runfiles = ctx.runfiles(files = local_files))]

_package_runtime_files = rule(
    implementation = __package_runtime_files_impl,
    attrs = {
        "runtime_files": attr.label(mandatory = True),
        "runtime_sibling_files": attr.label(mandatory = True),
        "runtime_qt_platforms_plugins_files": attr.label(mandatory = True),
    },
)

# When going through qt_cc_binary, we ensure
def qt_cc_binary(name, srcs, qt_dep, deps):
    runtime_files_name = name + "_runtime_files"
    _package_runtime_files(
        name = runtime_files_name,
        runtime_files = qt_dep + "_data_files",
        runtime_sibling_files = qt_dep + "_data_sibling_files",
        runtime_qt_platforms_plugins_files = qt_dep + "_platforms_plugins",
    )

    native.cc_binary(
        name = name,
        srcs = srcs,
        deps = [qt_dep + "_lib"] + deps,
        data = [":" + runtime_files_name],
    )

def __qt_resource_impl(ctx):
    # Strip the "/BUILD", there's probably a better way but I don't know what it
    # is.
    package_dir = ctx.build_file_path[:-5]

    linked_assets = []
    for x in ctx.files.srcs:
        if not x.path.startswith(package_dir):
            fail("Resources must be relative to package.")
        # The Qt rcc tool wants all the assets to be specified relative to the
        # qrc file, which we are generating.  So, symlink over all of its assets
        # so they are in trivial paths relative to the generated qrc file.
        out_path = ctx.actions.declare_file(x.path.replace(package_dir, ""))
        ctx.actions.symlink(output = out_path, target_file = x)
        linked_assets.append(out_path)

    file_list = "\n".join([
        '<file alias="{}">{}</file>'.format(x.path, x.path[len(package_dir):]) for x in ctx.files.srcs])

    qrc_content = """
        <RCC>
            <qresource prefix="{prefix}">
{file_list}
            </qresource>
        </RCC>
    """.format(prefix = ctx.attr.prefix, file_list = file_list)

    qrc_file = ctx.actions.declare_file(ctx.label.name + ".qrc")
    out_file = ctx.actions.declare_file(ctx.label.name + ".qrc.cc")

    ctx.actions.write(qrc_file, qrc_content)

    ctx.actions.run(
        outputs = [out_file],
        inputs = linked_assets + [qrc_file],
        tools = [ctx.executable.rcc],
        arguments = ["-name", ctx.label.name, "-o", out_file.path, qrc_file.path],
        executable = ctx.executable.rcc,
    )

    return DefaultInfo(files = depset([out_file]))


_qt_resource = rule(
    implementation = __qt_resource_impl,
    attrs = {
        "prefix": attr.string(mandatory = True, default = "/"),
        "srcs": attr.label_list(mandatory = True, allow_files = True),
        "rcc": attr.label(default = "@qt//:rcc", allow_single_file = True, executable = True, cfg = "exec"),
    },
)

def qt_resource(name, srcs, prefix="/"):
    cc_name = name + "_cc"
    _qt_resource(name = cc_name, srcs = srcs, prefix = prefix)
    native.cc_library(
        name = name,
        alwayslink = True,
        srcs = [":" + cc_name],
    )
