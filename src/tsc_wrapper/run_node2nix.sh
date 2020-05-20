#!/bin/bash
pushd .

THIS_SCRIPT_LOCATION="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"
cd $THIS_SCRIPT_LOCATION

rm -rf nix
mkdir -p nix

cd nix
cp ../package.json .
node2nix

popd
