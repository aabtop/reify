{ pkgsFunc ? import <nixpkgs>, inputInterfacePath }:

# Build our interface builder with our local version of the idt package, which
# most of this file's logic is devoted to adding to the haskellPackages
# attribute set.
let
  config = {
    packageOverrides = pkgs: rec {
      haskellPackages = pkgs.haskellPackages.override {
        overrides = haskellPackagesNew: haskellPackagesOld: {
          idt = import ../idt { haskellPackages = haskellPackagesOld; };
        };
      };
    };
  };
  pkgs = pkgsFunc { inherit config; };
in
  pkgs.haskellPackages.callCabal2nix "reify-input-interface" inputInterfacePath {}
