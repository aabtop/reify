#!/bin/bash

# This script is intended to be executed within a docker container created by
# the top-level build.sh/.bat scripts.
nix-build /src/projects/hypo/src -o /nix_out_symlink && cp /nix_out_symlink/bin/hypo /out
