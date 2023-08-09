{ inputs, pkgs, ... }:

{
  name = "marlowe-playground";


  env = {
    PLAYWRIGHT_BROWSERS_PATH = "${nix.marlowe-playground.playwright}"; 
    PLAYWRIGHT_SKIP_BROWSER_DOWNLOAD = "true"; 
  };


  scripts.generate-purescript = {
    exec = nix.marlowe-playground.scripts.generate-purescript;
    description = "Generate all PureScript Bridge code";
    group = "purescript";
  };

  scripts.update-client-deps = {
    exec = nix.marlowe-playground.scripts.generate-purescript;
    description = "Run spago2nix in marlowe-playground-client";
    group = "purescript";
  };

  scripts.generate-purescript = {
    exec = nix.marlowe-playground.scripts.start-backend;
    description = "Start the Marlowe Playground server";
    group = "general";
  };


  packages = [
    nix.marlowe-playground.purescript.purs-tidy
    nix.marlowe-playground.purescript.purs
    nix.marlowe-playground.purescript.spago
    nix.marlowe-playground.purescript.spago2nix
    nix.marlowe-playground.purescript.psa
    nix.marlowe-playground.purescript.purescript-language-server
    nix.marlowe-playground.purescript.pscid

    pkgs.jq
    pkgs.yq
    pkgs.gnused
    pkgs.act
    pkgs.gawk
    pkgs.nil
    pkgs.z3
    pkgs.nodejs-14_x
  ];

}