with import <nixpkgs> {};

let
  reifySrc = ../../../src;
  inputInterfacePath = ./interface;
  reifyProject = callPackage (reifySrc + /nix/project.nix) { inherit inputInterfacePath; };
  reify = callPackage (reifySrc + /nix/derivation.nix) { inherit reifyProject; };


  v8_no_icu = v8.overrideAttrs (attrs: {
    buildInputs = [ glib ];
    gnFlags =
      (lib.remove "is_debug=true" attrs.gnFlags) ++ [
          "v8_enable_i18n_support=false"
          "is_debug=false"
        ];
  });
in
  stdenv.mkDerivation {
    name = "hypo";

    inherit reifyProject;
    nativeBuildInputs = [cmake reifyProject];
    buildInputs = [
      v8_no_icu
      pkgsStatic.cgal_5
      pkgsStatic.gmp
      pkgsStatic.mpfr
      pkgsStatic.boost
      glibc.static
    ];

    src = ./.;

    enableParallelBuilding = true;
    
    cmakeFlags = ["-DREIFY_HYPO_PATH:PATH=${reifyProject}"];
  }
