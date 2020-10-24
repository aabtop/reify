def _vcpkg_impl(repository_ctx):
  release = "2020.07"
  repository_ctx.report_progress("Downloding vcpkg source...")
  repository_ctx.download_and_extract(
    url="https://github.com/microsoft/vcpkg/archive/{}.zip".format(release),
    output="vcpkg",
    sha256="2b2cea3ce07a3159f1c5a6d21bfa5e44fe5f8f62966093890ee0f0f5718f122c",
    stripPrefix="vcpkg-{}/".format(release),
  )
  repository_ctx.report_progress("Executing vcpkg bootstrap script...")
  repository_ctx.execute(
    ["vcpkg/bootstrap-vcpkg.bat"],
  )
  repository_ctx.report_progress("Calling vcpkg install...")
  repository_ctx.execute(
    ["vcpkg/vcpkg", "install"] + [x + ":x64-windows-static" for x in repository_ctx.attr.packages],
  )

  repository_ctx.file(
    "BUILD",
    content="\n".join(["""
cc_library(
  name = "{package}",
  hdrs = glob([
    "{package_directory}/include/**/*.h",
  ]),
  srcs = glob(["{package_directory}/**/*.lib","{package_directory}/**/*.pdb"]),
  includes = ["{package_directory}/include"],
  visibility = ["//visibility:public"],
)
    """.format(
            package=x,
            package_directory="vcpkg/packages/{}_x64-windows-static".format(x)) for x in repository_ctx.attr.packages]),
  )

vcpkg = repository_rule(
  implementation = _vcpkg_impl,
  attrs = {
    "packages": attr.string_list(
        mandatory=True,
        allow_empty=False,
        doc="List of packages to install via vcpkgs.  These will become available as Bazel targets in the root of the new repository."
    ),
  }
)
