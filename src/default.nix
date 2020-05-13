with import <nixpkgs> {};

let
  inputInterfacePath = ./test_data/hypo/interface;
  pkgs = rec {
    reifyProject = callPackage ./nix/project.nix { inherit inputInterfacePath; };
    reify = callPackage ./nix/derivation.nix { inherit reifyProject; };
  };
in
  pkgs.reify
