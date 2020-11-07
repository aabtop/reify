#!/bin/bash
set -euo pipefail

DEPOT_TOOLS_DIR=$(realpath ${1})
V8_GCLIENT_DIR=$(realpath ${2})
V8_BRANCH=${3}
V8_SRC_DIR=${V8_GCLIENT_DIR}/v8

# These can be convenient for debuggin the fetching process
#ln -s /v8/depot_tools ${DEPOT_TOOLS_DIR}
#ln -s /v8/v8 ${V8_GCLIENT_DIR}
#exit 0

if [ ! -d ${DEPOT_TOOLS_DIR} ]; then (
  git clone https://chromium.googlesource.com/chromium/tools/depot_tools.git ${DEPOT_TOOLS_DIR}

  cd ${DEPOT_TOOLS_DIR}
  ./gclient
) fi

export PATH=${DEPOT_TOOLS_DIR}:${PATH}

if [ ! -d ${V8_GCLIENT_DIR} ]; then (
  mkdir -p ${V8_GCLIENT_DIR}
  cd ${V8_GCLIENT_DIR}

  git clone --branch ${V8_BRANCH} https://github.com/v8/v8.git --depth 1

  cat <<EOM > .gclient
solutions = [
  { "name"        : 'v8',
    "url"         : 'https://github.com/v8/v8.git',
    "deps_file"   : 'DEPS',
    "managed"     : False,
    "custom_deps" : {},
    "custom_vars": {},
  },
]
EOM

  gclient sync --no-history --shallow
) fi
