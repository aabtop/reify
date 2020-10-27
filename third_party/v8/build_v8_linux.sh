#!/bin/bash
set -euo pipefail

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"
WORK_DIR=${SCRIPT_DIR}/build_linux
FETCH_DIR=${WORK_DIR}/fetch
OUT_DIR=${WORK_DIR}/out
PACKAGE_DIR=${WORK_DIR}/package

DEPOT_TOOLS_DIR=${FETCH_DIR}/depot_tools

V8_GCLIENT_DIR=${FETCH_DIR}/v8
V8_SRC_DIR=${V8_GCLIENT_DIR}/v8

BUILDS="linux-debug-x64-clang linux-release-x64-clang"

if [ ! -d ${DEPOT_TOOLS_DIR} ]; then (
  git clone https://chromium.googlesource.com/chromium/tools/depot_tools.git ${DEPOT_TOOLS_DIR}

  cd ${DEPOT_TOOLS_DIR}
  ./gclient
) fi

export PATH=${DEPOT_TOOLS_DIR}:${PATH}

if [ ! -d ${V8_GCLIENT_DIR} ]; then (
  mkdir -p ${V8_GCLIENT_DIR}
  cd ${V8_GCLIENT_DIR}

  cat <<EOM > .gclient
solutions = [
  { "name"        : 'v8',
    "url"         : 'https://github.com/v8/v8.git@8.6.395.10',
    "deps_file"   : 'DEPS',
    "managed"     : True,
    "custom_deps" : {
    },
    "custom_vars": {},
  },
]
EOM

  gclient sync --no-history --shallow
) fi

if [ ! -d ${OUT_DIR} ]; then (
  mkdir -p ${OUT_DIR}

  for B in ${BUILDS}
  do
    BUILD_DIR=${OUT_DIR}/$B

    mkdir ${BUILD_DIR}
    cat <<EOM > ${BUILD_DIR}/args.gn
is_component_build = false
v8_enable_backtrace = true
v8_enable_disassembler = true
v8_enable_object_print = true
v8_enable_verify_heap = true
v8_static_library = true
v8_enable_i18n_support = false
v8_use_external_startup_data = false
is_clang = true
EOM

    if [ $B == linux-debug-x64-clang ]; then (
      cat <<EOM >> ${BUILD_DIR}/args.gn
is_debug = true
is_official_build = false
EOM
    ) fi

    if [ $B == linux-release-x64-clang ]; then (
      cat <<EOM >> ${BUILD_DIR}/args.gn
is_debug = false
is_official_build = true
EOM
    ) fi

    cd ${V8_SRC_DIR}

    export DEPOT_TOOLS_UPDATE=0
    gn gen ${BUILD_DIR}

    ninja -C ${BUILD_DIR}
  done
) fi

if [ ! -d ${PACKAGE_DIR} ]; then (
  mkdir -p ${PACKAGE_DIR}

  for B in ${BUILDS}
  do
    mkdir -p ${PACKAGE_DIR}/${B}
    cd ${PACKAGE_DIR}/${B}

    cp -r ${V8_SRC_DIR}/include include
    mkdir lib
    cp ${OUT_DIR}/${B}/obj/*.a lib/
  done
) fi
