with import <nixpkgs> {};

let
  pkgs = rec {
    inputInterfacePath = ./test_data/hypo/interface;
    reifyProject = callPackage ./nix/project.nix { inherit inputInterfacePath; };
    reify = callPackage ./nix/derivation.nix { inherit reifyProject; };
  };
in
  pkgs.reify
