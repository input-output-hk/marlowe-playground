{ pkgs, system, easyPS, prettier }:
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
    nix run .#apps.${system}.marlowe-playground-server:exe:marlowe-playground-server -- psgenerator $generated

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
in
{
  inherit marlowePlaygroundGeneratePurs;
}
