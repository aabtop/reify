# tsc_wrapper

This folder contains the TypeScript code that wraps the Microsoft TypeScript
Compiler and interfaces with the C++/V8 side of Reify to communicate the
results.

## In case of failed builds...

Note that because Nix requires package hashes ahead of time to ensure
deterministic builds with regards to network fetches, you *must* run
`./run_node2nix.sh` in this root `tsc_wrapper/` folder every time you change
`package.json`.  If not, the new packages will not be available during the Nix
build.

