# V8 in Reify

While the logic here could be contained within a Bazel repository rule, it takes
so long to download the V8 source code (and all its dependencies) that it's just
nicer to keep V8 updates separate and explicit.

Use the `build_v8_PLATFORM.sh/bat` file to download and build V8 on your
platform. You will need to do this before calling Bazel.  The output will be a
`build_PLATFORM` directory (in the source tree) containing the V8 source code
and the compiled output.
