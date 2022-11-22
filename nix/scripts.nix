{ pkgs, easyPS, prettier }:
let
  writeShellScriptBinInRepoRoot = name: script: pkgs.writeShellScriptBin name ''
    cd `${pkgs.git}/bin/git rev-parse --show-toplevel`
    ${script}
  '';

  marlowePlaygroundGeneratePurs = writeShellScriptBinInRepoRoot "marlowe-playground-generate-purs" ''
    #!/bin/bash
    generated=./marlowe-playground-client/generated

    # Clean old build
    rm -rf $generated

    # Re-generate the files
    mkdir -p $generated
    nix run .#marlowe-playground-server:exe:marlowe-playground-server -- psgenerator $generated

    # Format them
    cp ${builtins.path { name = "tidyrc.json"; path = ../.tidyrc.json; } } $generated/.tidyrc.json
    cp ${builtins.path { name = "tidyoperators"; path = ../.tidyoperators; } } $generated/.tidyoperators
    cd $generated
    ${easyPS.purs-tidy}/bin/purs-tidy format-in-place *
    ${prettier}/bin/prettier -w .
    rm -f .tidyrc.json
    rm -f .tidyoperators
    cd ../..
    chmod -R +w $generated
  '';

  build-ghc-with-marlowe = "$(nix build .#ghc-with-marlowe --print-out-paths)";

  # start-backend: script to start the plutus-playground-server
  #
  # Note: We need to add ghc to the path because the server provides /runghc
  # which needs ghc and dependencies.
  startBackend = writeShellScriptBinInRepoRoot "marlowe-playground-server" ''
    echo "marlowe-playground-server: for development use only"
    GHC_WITH_PKGS=${build-ghc-with-marlowe}
    export PATH=$GHC_WITH_PKGS/bin:$PATH
    export FRONTEND_URL=https://localhost:8009
    nix run .#marlowe-playground-server:exe:marlowe-playground-server -- webserver
  '';
in
{
  inherit marlowePlaygroundGeneratePurs startBackend;
}
