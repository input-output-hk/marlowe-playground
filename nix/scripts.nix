{ pkgs, easyPS, prettier, writeShellScriptBinInRepoRoot }:
let

  # Output containing the purescript bridge code
  generated-purescript = pkgs.runCommand "marlowe-playground-purescript" { } ''
    mkdir $out
    ${pkgs.playground.hsPkgs.marlowe-playground-server.components.exes.marlowe-playground-server}/bin/marlowe-playground-server psgenerator $out
    cp ${builtins.path { name = "tidyrc.json"; path = ../.tidyrc.json; } } $out/.tidyrc.json
    cp ${builtins.path { name = "tidyoperators"; path = ../.tidyoperators; } } $out/.tidyoperators
    cd $out
    ${easyPS.purs-tidy}/bin/purs-tidy format-in-place *
    ${prettier}/bin/prettier -w .
    rm -f $out/.tidyrc.json
    rm -f $out/.tidyoperators
  '';

  # generate-purescript = writeShellScriptBinInRepoRoot "marlowe-playground-generate-purs" ''
  #   generated=./marlowe-playground-client/generated
  #   rm -rf $generated
  #   cp -a $(nix-build -A marlowe-playground.generated-purescript --no-out-link "$@") $generated
  #   chmod -R +w $generated
  # '';


  marlowe-playground-generate-purs = writeShellScriptBinInRepoRoot "marlowe-playground-generate-purs" ''
    generated=./marlowe-playground-client/generated

    # Clean old build
    rm -rf $generated

    # Re-generate the files
    cp -a $(nix build .#generated-purescript --no-link --print-out-paths) $generated
    chmod -R +w $generated
  '';

  build-ghc-with-marlowe = "$(nix build .#ghc-with-marlowe --no-link --print-out-paths)";

  # start-backend: script to start the plutus-playground-server
  #
  # Note: We need to add ghc to the path because the server provides /runghc
  # which needs ghc and dependencies.
  start-backend = writeShellScriptBinInRepoRoot "marlowe-playground-server" ''
    echo "marlowe-playground-server: for development use only"
    GHC_WITH_PKGS=${build-ghc-with-marlowe}
    export PATH=$GHC_WITH_PKGS/bin:$PATH
    export FRONTEND_URL=https://localhost:8009
    nix run .#marlowe-playground-server:exe:marlowe-playground-server -- webserver
  '';

  update-client-deps = writeShellScriptBinInRepoRoot "update-client-deps" ''
    cd marlowe-playground-client
    ${easyPS.spago2nix}/bin/spago2nix generate
  '';
in
{
  inherit marlowe-playground-generate-purs start-backend generated-purescript update-client-deps;
}
