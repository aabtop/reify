with import <nixpkgs> {};

let
  inputInterfacePath = ./interface;
  reifyProject = callPackage ../../nix/project.nix { inherit inputInterfacePath; };
  reify = callPackage ../../nix/derivation.nix { inherit reifyProject; };

in
  stdenv.mkDerivation {
    name = "hypo";

    inherit reifyProject;
    nativeBuildInputs = [cmake reifyProject];
    buildInputs = [v8];

    src = ./.;

    enableParallelBuilding = true;
    
    cmakeFlags = ["-DREIFY_HYPO_PATH:PATH=${reifyProject}"];
  }
