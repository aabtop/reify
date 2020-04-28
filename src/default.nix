with import <nixpkgs> {};

let
  pkgs = rec {
    memdesc = callPackage ./nix/memdesc.nix {};
    reify-interface = callPackage ./interface {};
    reify = callPackage ./nix/derivation.nix { inherit memdesc; inherit reify-interface; };
  };
in
  pkgs.reify
