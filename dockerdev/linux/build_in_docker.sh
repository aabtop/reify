#!/bin/bash
set -euo pipefail

echo "In container, building..."

export BUILD_CONFIG=opt

cd /src

cd projects/hypo
bazel build //src/ide //:hypo -c $BUILD_CONFIG --symlink_prefix=/bazel- --verbose_failures

# Strip symbols from the output executable and place the results in the
# output directory.
strip -s /bazel-bin/hypo -o /out/hypo
