{ pkgsFunc ? import <nixpkgs>, inputInterfacePath }:

# Build our interface builder with our local version of the idt package, which
# most of this file's logic is devoted to adding to the haskellPackages
# attribute set.
let
  config = {
    packageOverrides = pkgs: rec {
      haskellPackages = pkgs.haskellPackages.override {
        overrides = haskellPackagesNew: haskellPackagesOld: {
          idt = import ../third_party/idt { haskellPackages = haskellPackagesOld; };
          reify-input-interface = import ./input_interface.nix {
            inherit pkgsFunc;
            inherit inputInterfacePath;
          };
        };
      };
    };
  };
  pkgs = pkgsFunc { inherit config; };
  interfaceBuilderExecutables = pkgs.haskellPackages.callCabal2nix "reify-interface-builder" ./. {};
  configJsonFile =
      pkgs.runCommand "reify-interface-config-json"
                      { buildInputs = [ interfaceBuilderExecutables ]; }
                      "${interfaceBuilderExecutables}/bin/reify-interface-config-dump-json > $out";
  interfaceDefinitionFiles =
      pkgs.runCommand "reify-interface-definition-files"
                      { buildInputs = [ interfaceBuilderExecutables ]; }
                      "mkdir -p $out && ${interfaceBuilderExecutables}/bin/reify-interface-builder $out";
in
  rec {
    inherit interfaceDefinitionFiles;
    inherit inputInterfacePath;
    config = (builtins.fromJSON ( import configJsonFile ));
  }
