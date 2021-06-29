#!/bin/bash
set -euo pipefail

echo "In container, building..."

export BUILD_CONFIG=opt

export OUTPUT_DIR="/out"

cd /src

cd projects/hypo
bazel build //:hypo //:ide //:visualizer -c $BUILD_CONFIG --symlink_prefix=/bazel- --verbose_failures

rm -rf ${OUTPUT_DIR}/*

# Strip symbols from the output executable and place the results in the
# output directory.
strip -s /bazel-bin/hypo -o ${OUTPUT_DIR}/hypo

cp -rL /bazel-bin/ide.runfiles/hypo/* ${OUTPUT_DIR}
chmod +w ${OUTPUT_DIR}/ide
strip -s ${OUTPUT_DIR}/ide

cp -rL /bazel-bin/visualizer.runfiles/hypo/* ${OUTPUT_DIR}
chmod +w ${OUTPUT_DIR}/visualizer
strip -s ${OUTPUT_DIR}/visualizer

# Bazel automatically sets all files as executable, even if they're not.
# (See https://github.com/bazelbuild/bazel/issues/6530#issuecomment-435779225).
# This isn't a big deal, but aesthetically it's nice to have our top-level files
# have executable permissions set correctly so that when they are listed with
# color coding, it is evident what is executable.
# Currently, there is only this one non-executable top-level file.
chmod -x ${OUTPUT_DIR}/qt.conf