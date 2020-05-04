with import <nixpkgs> {
  config = {
    packageOverrides = pkgs: {
      v8 = pkgs.v8.overrideAttrs
        (oldAttrs: rec {
          gnFlags = oldAttrs.gnFlags ++ [''custom_toolchain="//build/toolchain/linux/unbundle:default"''];
          ninjaFlags = oldAttrs.ninjaFlags ++ ["mksnapshot"];
          installPhase = "install -D mksnapshot $out/bin/mksnapshot\n" + oldAttrs.installPhase;
        });
    };
  };
};

let
  pkgs = rec {
    memdesc = callPackage ./nix/memdesc.nix {};
    reify-interface = callPackage ./interface {};
    reify = callPackage ./nix/derivation.nix { inherit memdesc; inherit reify-interface; };
  };
in
  pkgs.reify
