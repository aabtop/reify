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
  repository_ctx.report_progress("Calling `vcpkg install {}`...".format(repository_ctx.attr.package))
  repository_ctx.execute(
    ["vcpkg/vcpkg", "install", repository_ctx.attr.package + ":x64-windows-static"],
  )

  repository_ctx.file(
    "BUILD",
    content="""
cc_library(
  name = "package",
  hdrs = glob([
    "{package_directory}/include/**/*.h",
  ]),
  srcs = glob(["{package_directory}/**/*.lib","{package_directory}/**/*.pdb"]),
  includes = ["{package_directory}/include"],
  visibility = ["//visibility:public"],
)
    """.format(package_directory="vcpkg/packages/{}_x64-windows-static".format(repository_ctx.attr.package)),
  )

vcpkg = repository_rule(
  implementation = _vcpkg_impl,
  attrs = {
    "package": attr.string(mandatory=True),
  }
)
