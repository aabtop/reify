{ haskellPackages ? (import <nixpkgs> {}).haskellPackages }:

# This package is just a simple Cabal-based library.
haskellPackages.callCabal2nix "idt" ./. {}
