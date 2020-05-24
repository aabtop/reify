with import <nixpkgs> {};

let
  reifySrc = ../../../src;
  inputInterfacePath = ./interface;
  reifyProject = callPackage (reifySrc + /nix/project.nix) { inherit inputInterfacePath; };
  reify = callPackage (reifySrc + /nix/derivation.nix) { inherit reifyProject; };

  v8_no_icu = v8.overrideAttrs (attrs: {
    buildInputs = [ glib ];
    gnFlags =
        (lib.remove "v8_enable_i18n_support=true" attrs.gnFlags) ++ [
          "v8_enable_i18n_support=false"
        ];
  });
  v8_release = v8_no_icu.overrideAttrs (attrs: {
    gnFlags =
      (lib.remove "is_debug=true" (
              lib.remove "is_official_build=false" attrs.gnFlags)) ++ [
          "is_debug=false"
          "is_official_build=true"
          "is_cfi=false"
        ];
  });
  v8_debug = v8_no_icu.overrideAttrs (attrs: {
    dontStrip = true;
  });
in
  stdenv.mkDerivation {
    name = "hypo";

    inherit reifyProject;
    nativeBuildInputs = [cmake reifyProject];
    buildInputs = [
      v8_release
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
