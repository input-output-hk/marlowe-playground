{ inputs, cell, pkgs, ... }:
let
  inherit (cell) library packages scripts;
  inherit (library) pkgs haskell-nix;
in 
{
  name = "marlowe-playground";

  env = {
    PLAYWRIGHT_BROWSERS_PATH = "${packages.playwright}"; 
    PLAYWRIGHT_SKIP_BROWSER_DOWNLOAD = "true"; 
  };


  scripts = {

    exec = ''
      # Bash code to be executed whenever the script `foobar` is run.
      echo "Delete me from your nix/shell.nix!"
    '';
    description = ''
      You might want to delete the foobar script.
    '';
    group = "bazwaz";
    enable = true;

  }
  # {
  #   package = scripts.generate-purescript;
  #   category = "purescript";
  #   help = "Generate all PureScript Bridge code";
  # }
  # {
  #   package = scripts.start-backend;
  #   category = "general commands";
  #   help = "Start the Marlowe Playground server";
  # }
  # {
  #   package = scripts.update-client-deps;
  #   category = "purescript";
  #   help = "Run spago2nix in marlowe-playground-client";
  # }

  packages = [
    pkgs.jq
    pkgs.yq
    pkgs.gnused
    pkgs.act
    pkgs.gawk
    pkgs.nil
    pkgs.z3
    pkgs.nodejs-14_x

    nix.marlowe-cardano.purescript.purs-tidy
    nix.marlowe-cardano.purescript.purs
    nix.marlowe-cardano.purescript.spago
    nix.marlowe-cardano.purescript.spago2nix
    nix.marlowe-cardano.purescript.psa
    nix.marlowe-cardano.purescript.purescript-language-server
    nix.marlowe-cardano.purescript.pscid
  ];

}