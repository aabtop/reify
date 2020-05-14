with import <nixpkgs> {};

let
  inputInterfacePath = ./interface;
  reifyProject = callPackage ../../nix/project.nix { inherit inputInterfacePath; };
  reify = callPackage ../../nix/derivation.nix { inherit reifyProject; };

in
  stdenv.mkDerivation {
    name = "hypo";

    inherit reifyProject;
    nativeBuildInputs = [cmake reifyProject cgal_5 gmp mpfr boost];
    buildInputs = [v8];

    src = ./.;

    enableParallelBuilding = true;
    
    cmakeFlags = ["-DREIFY_HYPO_PATH:PATH=${reifyProject}"];
  }
