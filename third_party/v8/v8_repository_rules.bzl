def _fetch_v8_impl(repository_ctx):
    depot_tools_subdir = "depot_tools"
    v8_subdir = "v8"

    result = repository_ctx.execute(
        [repository_ctx.path(repository_ctx.attr._fetch_script), depot_tools_subdir, v8_subdir, repository_ctx.attr.branch],
    )
    if result.return_code:
        print(result.stdout)
        print(result.stderr)
        fail("Error fetching V8 repository")

    repository_ctx.template(
        "BUILD",
        repository_ctx.attr._build_template,
    )

fetch_v8 = repository_rule(
    implementation = _fetch_v8_impl,
    attrs = {
        "branch": attr.string(mandatory = True, doc = "The tag or branch of the V8 repository to clone."),
        "_fetch_script": attr.label(default = "@reify//:third_party/v8/fetch_linux.sh"),
        "_build_template": attr.label(default = "@reify//third_party/v8:v8.BUILD"),
    },
)
