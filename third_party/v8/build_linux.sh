#!/bin/bash
set -euo pipefail

DEPOT_TOOLS_DIR=$(realpath ${1})
V8_SRC_DIR=$(realpath ${2})
BUILD_CONFIG=${3}
OUTPUT_FILE=${4}

export PATH=${DEPOT_TOOLS_DIR}:${PATH}

OUT_DIR=out

if [ ! -d ${OUT_DIR} ]; then (
  mkdir -p ${OUT_DIR}

  cat <<EOM > ${OUT_DIR}/args.gn
is_component_build = false
v8_enable_backtrace = true
v8_enable_disassembler = true
v8_enable_object_print = true
v8_enable_verify_heap = true
v8_static_library = true
v8_enable_i18n_support = false
v8_use_external_startup_data = false
is_clang = true
clang_use_chrome_plugins = false
v8_monolithic = true
target_cpu = "x64"
use_custom_libcxx=false
EOM

  if [ ${BUILD_CONFIG} == debug ]; then (
    cat <<EOM >> ${OUT_DIR}/args.gn
is_debug = true
is_official_build = false
EOM
  ) fi

  if [ ${BUILD_CONFIG} == release ]; then (
    cat <<EOM >> ${OUT_DIR}/args.gn
is_debug = false
is_official_build = true
is_cfi = false  # Without this, the linker will produce many unresolved symbols.
EOM
  ) fi

  OUT_DIR=$(realpath ${OUT_DIR})

  cd ${V8_SRC_DIR}

  export DEPOT_TOOLS_UPDATE=0
  gn gen ${OUT_DIR}

  ninja -C ${OUT_DIR} v8_monolith > ${OUT_DIR}/ninja_output.txt 2>&1
) fi

cp ${OUT_DIR}/obj/$(basename ${OUTPUT_FILE}) ${OUTPUT_FILE}
