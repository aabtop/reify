
def _v8_library_impl(ctx):
  build_dir_base_path = "out/x64.release"
#  build_dir = ctx.actions.declare_directory(build_dir_base_path)
  args_gn_file = ctx.actions.declare_file(build_dir_base_path + "/args.gn")
  foo = ctx.actions.declare_file(build_dir_base_path + "/foo.cc")

#  ctx.actions.run(
#    outputs = [build_dir],
#    arguments = ["-p", build_dir.path],
#    executable = "mkdir",
#  )

  ctx.actions.write(
    args_gn_file,
    """
is_component_build = false
is_debug = false
v8_enable_backtrace = true
v8_enable_disassembler = true
v8_enable_object_print = true
v8_enable_verify_heap = true
    """
  )


#  ctx.actions.run(
#      inputs = [args_gn_file] + ctx.files.srcs,
#      outputs = [foo],
#      arguments = ["gen", args_gn_file.dirname],
#      executable = ctx.executable.gn,
#  )

  ctx.actions.run_shell(
      inputs = [args_gn_file] + ctx.files.srcs,
      outputs = [foo],
      tools = [ctx.executable.gn],
      command = """
      set -eou pipefail
      export DEPOT_TOOLS_UPDATE=0
      export DEPOT_TOOLS_WIN_TOOLCHAIN=0
      export PATH={depot_tools_path}:$PATH
      export PYTHONPATH={depot_tools_path}
      cd {root_v8_src_dir}
      ls 1>&2
      gn gen {outdir}
      """.format(
          root_v8_src_dir=ctx.files.root.path,
          depot_tools_path=ctx.executable.gn.dirname,
          outdir=args_gn_file.dirname),
  )

  return [
      CcInfo(compilation_context = cc_common.create_compilation_context(
          headers = depset(direct = [foo]),
          includes = depset(direct = ["init/"]),
      )),
      DefaultInfo(
          files = depset([foo])
      ) 
  ]


v8_library = rule(
  implementation = _v8_library_impl,
  attrs = {
    "root": attr.label(
        doc="The list of sources used to build V8.",
        allow_files=True,
        mandatory=True,),
    "gn": attr.label(
        doc="Location of `gn` executable.",
        executable=True,
        allow_files=True,
        cfg = "exec",
        mandatory=True,)
  },
)
