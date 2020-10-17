def prepend_depot_tools_args(repository_ctx):
  if "windows" in repository_ctx.os.name.lower():
    return ["cmd", "/c"]
  else:
    return []

def __install_depot_tools_impl(repository_ctx):
  repository_ctx.report_progress("Downloading and extracting depot tools bundle.")
  repository_ctx.download_and_extract(
    url="https://storage.googleapis.com/chrome-infra/depot_tools.zip",
    output=".",
  )

  repository_ctx.report_progress("Running depot_tools' self updater.")
  result = repository_ctx.execute(
    prepend_depot_tools_args(repository_ctx) + ["gclient"],
  )
  if result.return_code != 0:
    fail(result.stderr)

  repository_ctx.symlink(repository_ctx.attr.build_file, "BUILD")


# Unfortunately this isn't hermetic because of a myriad of issues when
# attempting to pin the depot_tools version.  depot_tools seems to be very
# unhappy when its self-updater is not run.
install_depot_tools = repository_rule(
    implementation = __install_depot_tools_impl,
    attrs = {
      "build_file": attr.label(doc="Location of the depot_tools.BUILD file.", mandatory=True,),
    },
)
