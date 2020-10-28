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

  BOOTSTRAP_SCRIPT = None
  if "windows" in repository_ctx.os.name:
    BOOTSTRAP_SCRIPT = "vcpkg/bootstrap-vcpkg.bat"
  else:
    BOOTSTRAP_SCRIPT = "vcpkg/bootstrap-vcpkg.sh"
  result = repository_ctx.execute(
    [BOOTSTRAP_SCRIPT],
  )
  if result.return_code:
    fail("Error executing vcpkg bootstrap script.")

  triplet_os = None
  if "windows" in repository_ctx.os.name:
    triplet_os = "windows-static"
  else:
    triplet_os = "linux"

  repository_ctx.report_progress("Calling vcpkg install...")
  print("execute: " + str(["vcpkg/vcpkg", "install"] + [x + ":x64-{os}".format(os=triplet_os) for x in repository_ctx.attr.packages]))
  result = repository_ctx.execute(
    ["vcpkg/vcpkg", "install"] + [x + ":x64-{os}".format(os=triplet_os) for x in repository_ctx.attr.packages],
  )
  if result.return_code:
    fail("Error executing vcpkg install step.")

  repository_ctx.file(
    "BUILD",
    content="""
config_setting(
    name = "dbg_mode",
    values = {
        "compilation_mode": "dbg",
    },
)
    """ + "\n".join(["""

cc_library(
  name = "{package}",
  hdrs = glob([
    "{package_directory}/include/**/*.h",
  ]),
  srcs = select({{
    ":dbg_mode": glob([
        "{package_directory}/debug/lib/**/*.lib",
        "{package_directory}/lib/**/*.pdb",
    ]),
    "//conditions:default": glob([
        "{package_directory}/lib/**/*.lib",
    ]),
  }}),
  linkopts = select({{
    ":dbg_mode": [],
    # No PDB data available in release mode.
    "//conditions:default": ["/ignore:4099"],
  }}),
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
