with import <nixpkgs> {};

let
  pkgs = rec {
    memdesc = callPackage ./nix/memdesc.nix {};
    reify = callPackage ./nix/derivation.nix { inherit memdesc; };
  };
in
  pkgs.reify
