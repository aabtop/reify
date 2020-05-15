with import <nixpkgs> {};

let
  reifySrc = ../../../src;
  inputInterfacePath = ./interface;
  reifyProject = callPackage (reifySrc + /nix/project.nix) { inherit inputInterfacePath; };
  reify = callPackage (reifySrc + /nix/derivation.nix) { inherit reifyProject; };

in
  stdenv.mkDerivation {
    name = "hypo";

    inherit reifyProject;
    nativeBuildInputs = [cmake reifyProject];
    buildInputs = [v8 cgal_5 gmp mpfr boost];

    src = ./.;

    enableParallelBuilding = true;
    
    cmakeFlags = ["-DREIFY_HYPO_PATH:PATH=${reifyProject}"];
  }
