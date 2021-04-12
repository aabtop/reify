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
chmod +x /out/hypo

IDE_DIR="/out/ide"
if [ -d ${IDE_DIR} ]; then
    rm -rf ${IDE_DIR}
fi

mkdir -p ${IDE_DIR}
cp -rL /bazel-bin/ide.runfiles/hypo/* ${IDE_DIR}
strip -s ${IDE_DIR}/ide
chmod +x ${IDE_DIR}/ide
chmod +x ${IDE_DIR}/qt/libexec/QtWebEngineProcess