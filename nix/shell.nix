# This file is part of the IOGX template and is documented at the link below:
# https://www.github.com/input-output-hk/iogx#34-nixshellnix

{ nix, inputs', pkgs, system, l, ... }:

{
  name = "marlowe-playground";


  env = {
    PLAYWRIGHT_BROWSERS_PATH = 
      l.optionalString (system == "x86_64-linux") 
        "${nix.marlowe-playground.playwright}"; 

    PLAYWRIGHT_SKIP_BROWSER_DOWNLOAD = "true"; 
  };


  scripts.generate-purescript = {
    exec = nix.marlowe-playground.scripts.generate-purescript;
    description = "Generate all PureScript Bridge code";
    group = "purescript";
  };

  scripts.update-client-deps = {
    exec = nix.marlowe-playground.scripts.update-client-deps;
    description = "Run spago2nix in marlowe-playground-client";
    group = "purescript";
  };

  scripts.start-backend = {
    exec = nix.marlowe-playground.scripts.start-backend;
    description = "Start the Marlowe Playground server";
    group = "general";
  };


  packages = [
    nix.marlowe-playground.easy-purescript-nix.purs-tidy
    nix.marlowe-playground.easy-purescript-nix.purs
    nix.marlowe-playground.easy-purescript-nix.spago
    nix.marlowe-playground.easy-purescript-nix.spago2nix
    nix.marlowe-playground.easy-purescript-nix.psa
    nix.marlowe-playground.easy-purescript-nix.purescript-language-server
    nix.marlowe-playground.easy-purescript-nix.pscid

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