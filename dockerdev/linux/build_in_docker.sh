#!/bin/bash
set -euo pipefail

echo "In container, building..."

export BUILD_CONFIG=opt

cd /src

cd projects/hypo
bazel build //:hypo //:ide -c $BUILD_CONFIG --symlink_prefix=/bazel- --verbose_failures

# Strip symbols from the output executable and place the results in the
# output directory.
strip -s /bazel-bin/hypo -o /out/hypo

IDE_DIR="/out/ide"
if [ -d ${IDE_DIR} ]; then
    rm -rf ${IDE_DIR}
fi

mkdir -p ${IDE_DIR}
cp -rL /bazel-bin/ide.runfiles/hypo/* ${IDE_DIR}
strip -s ${IDE_DIR}/ide
# Bazel automatically sets all files as executable, even if they're not.
# (See https://github.com/bazelbuild/bazel/issues/6530#issuecomment-435779225).
# This isn't a big deal, but aesthetically it's nice to have our top-level files
# have executable permissions set correctly so that when they are listed with
# color coding, it is evident what is executable.
# Currently, there is only this one non-executable top-level file.
chmod -x ${IDE_DIR}/qt.conf