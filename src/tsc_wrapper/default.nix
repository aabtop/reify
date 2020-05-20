{runCommand, nodePackages}:

let
  # First we copy the package.json out and isolate it, so that we can fetch
  # depending only on package.json and nothing else.  This is done because
  # fetching the dependencies can take a very long time, and it's nice to not
  # have to do this if we're just changing local source code.
  isolatePackageJson =
    runCommand "reify-package-copy" { src = ./package.json; } "cp $src $out";
  setupNode2Nix =
    runCommand "reify-setup-node-2-nix" {
          buildInputs = [nodePackages.node2nix]; inherit isolatePackageJson;
        }
        ''
          mkdir -p $out
          cd $out
          cp $isolatePackageJson package.json
          node2nix
        '';
  node2NixDerivation = (import setupNode2Nix {}).package;
in
  runCommand
      "generate-tsc_wrapper-binary"
      {
        inherit node2NixDerivation;
        src = ./.;
      }
      # Overlay our original source code over top the fetched node_modules
      # directory, and then call webpack on it.
      ''
        cp --no-preserve=mode,ownership -r $src/* .
        cp -r $node2NixDerivation/lib/node_modules/tsc_wrapper/* .
        node_modules/.bin/webpack
        cp dist/tsc_wrapper.js $out
      ''
